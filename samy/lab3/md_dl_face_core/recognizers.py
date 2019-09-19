import abc
import logging

import numpy as np
from sklearn.externals import joblib

from md_dl_face_core.descriptors import FaceDescriptor, FaceDescriptorType
from md_dl_face_core.detectors import FaceDetector, FaceDetectorType
from md_dl_face_core.utils import is_sharp_enough, is_big_enough, is_big_enough_relative
from md_foundation.config.configuration_manager import ConfigurationMixin
from md_foundation.utils.common import is_valid_pose, get_subclasses

logger = logging.getLogger(__name__)

UNKNOWN_CLASS_NAME = 'unknown'


def match_type_to_class(cls, cls_type):
    classes = [sub for sub in get_subclasses(cls, depth=-1) if sub.type == cls_type]
    if len(classes) == 0:
        raise ValueError('Given type {cls_type} needs to be configured in the corresponding class or should be removed from {cls}!'.format(cls_type=cls_type, cls=cls))
    elif len(classes) > 1:
        raise ValueError('Given type {cls_type} is configured for multiple classes ({classes})!'.format(cls_type=cls_type, classes=[c.__name__ for c in classes]))
    subcls = classes[0]
    logger.debug('Matched class type {cls_type} to class {cls}'.format(cls_type=cls_type, cls=subcls.__name__))
    return subcls


class FaceRecognizer(abc.ABC, ConfigurationMixin):
    """
    Abstract basic wrapper class to define a full face recognition pipeline.
    """

    def __init__(self, detector=None, descriptor=None, classifier=None, config=None):
        logger.debug('Loading {}'.format(self.name))
        self.load_config(config=config)
        self._load_detector(detector=detector)
        self._load_descriptor(descriptor=descriptor)
        self._load_classifier(classifier=classifier)

    def _load_detector(self, detector=None):
        logger.debug('Loading detector')
        if detector is not None:
            logger.debug('from passed argument')
            self._detector = detector
        else:
            logger.debug('from configuration')
            detector_type_str = self.config.data['faceRecognizer.detectorType'].upper()
            try:
                detector_type = FaceDetectorType[detector_type_str]
                detector_cls = match_type_to_class(FaceDetector, detector_type)
                self._detector = detector_cls(config=self._config)
            except KeyError:
                raise ValueError("Unknown detector type '{}' in config! Valid values are: {}.".format(detector_type_str, [t.name for t in FaceDetectorType]))

    def _load_descriptor(self, descriptor=None):
        logger.debug('Loading descriptor')
        if descriptor is not None:
            logger.debug('from passed argument')
            self._descriptor = descriptor
        else:
            logger.debug('from configuration')
            descriptor_type_str = self.config.data['faceRecognizer.descriptorType'].upper()
            try:
                descriptor_type = FaceDescriptorType[descriptor_type_str]
                descriptor_cls = match_type_to_class(FaceDescriptor, descriptor_type)
                self._descriptor = descriptor_cls(config=self._config)
            except KeyError:
                raise ValueError("Unknown descriptor type '{}' in config! Valid values are: {}.".format(descriptor_type_str, [t.name for t in FaceDescriptorType]))

    def _load_classifier(self, classifier=None):
        logger.debug('Loading classifier')
        if classifier is not None:
            logger.debug('from passed argument')
            self._classifier = classifier
        else:
            logger.debug('from configuration')
            classifier_path = self.config.data['faceRecognizer.classifierPath']
            self._classifier = joblib.load(classifier_path)
            self._classifier.load_config(config=self._config)

    @property
    def name(self):
        return self.__class__.__name__

    @property
    def detector(self):
        return self._detector

    @property
    def descriptor(self):
        return self._descriptor

    @property
    def classifier(self):
        return self._classifier

    def forward(self, image, **kwargs):
        logger.debug('Pushing image through recognition pipeline')
        labels_rects = self._forward(image, **kwargs)
        predictions = self._parse_forward(labels_rects)
        return predictions

    def forward_proba(self, image, **kwargs):
        logger.debug('Pushing image through recognition pipeline (with confidence)')
        confs_rects = self._forward_proba(image, **kwargs)
        predictions = self._parse_forward_proba(confs_rects)
        return predictions

    @abc.abstractmethod
    def _parse_forward(self, labels_rects, **kwargs):
        pass

    @abc.abstractmethod
    def _parse_forward_proba(self, confs_rects, **kwargs):
        pass

    @abc.abstractmethod
    def _forward(self, image, **kwargs):
        pass

    @abc.abstractmethod
    def _forward_proba(self, image, **kwargs):
        pass


class GenericFaceRecognizer(FaceRecognizer):
    """
    Generic implementation of a face detection -> alignment -> classification pipeline.
    """

    def _forward_pass(self, image):
        rects = self.detector.detect(image)
        rects = [r for r in rects if is_big_enough(r, threshold=self.config.data['faceRecognizer.minimumBoxSize'])]
        rects = [r for r in rects if is_big_enough_relative(r, image, threshold=self.config.data['faceRecognizer.minimumBoxSizeRelative'])]
        try:
            poses = [self.detector.pose(image, r) for r in rects]
            rects = [r for r, p in zip(rects, poses) if is_valid_pose(p, max_pitch=self.config.data['faceRecognizer.maxPitch'], max_yaw=self.config.data['faceRecognizer.maxYaw'], max_roll=self.config.data['faceRecognizer.maxRoll'])]
        except NotImplementedError:
            logger.debug("Function 'pose' is not implemented for class {}".format(self.detector.name))
        faces = [self.detector.align(image, r) for r in rects]
        faces_rects = [(f, b) for f, b in zip(faces, rects) if f is not None]
        faces_rects = [(f, b) for f, b in faces_rects if is_sharp_enough(f, threshold=self.config.data['faceRecognizer.sharpnessThreshold'])]
        reps_rects = [(self.descriptor.represent(f), r) for f, r in faces_rects]
        return reps_rects

    def _forward(self, image, **kwargs):
        reps_rects = self._forward_pass(image)
        labels_rects = [(self.classifier.classify(e), r) for e, r in reps_rects]
        return labels_rects

    def _forward_proba(self, image, **kwargs):
        reps_rects = self._forward_pass(image)
        confs_rects = [(self.classifier.classify_proba(e), r) for e, r in reps_rects]
        return confs_rects

    def _parse_forward(self, labels_rects, **kwargs):
        predictions = []
        for label, rect in labels_rects:
            prediction = {
                'id': label,
                'confidence': None,
                'bbox': rect
            }
            predictions.append(prediction)
        return predictions

    def _parse_forward_proba(self, confs_rects, **kwargs):
        predictions = []
        for confs, rect in confs_rects:
            conf_max = float(np.max(confs))
            if conf_max < self.config.data.get('faceRecognizer.confidenceThreshold', 0.2):
                label = UNKNOWN_CLASS_NAME
            else:
                conf_max_ind = np.argmax(confs)
                label = self.classifier.transform_label_num(conf_max_ind)
            prediction = {
                'id': label,
                'confidence': conf_max,
                'bbox': rect
            }
            predictions.append(prediction)
        return predictions
