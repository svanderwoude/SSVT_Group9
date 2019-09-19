import abc
import logging
import os
import re
from enum import Enum

import cv2
import numpy as np

from md_foundation.config.configuration_manager import ConfigurationMixin
from md_foundation.utils.common import prewhiten

logger = logging.getLogger(__name__)

try:
    import caffe
    import tensorflow as tf
except ImportError as e:
    logger.error('Unable to import module: {}'.format(e))


class FaceDescriptorType(Enum):
    LIGHTCNN = 1
    FACENET = 2


class FaceDescriptor(abc.ABC, ConfigurationMixin):
    """
    Abstract base wrapper class for different types of face descriptors.
    """
    def __init__(self, config=None):
        logger.debug('Loading {}'.format(self.name))
        self.load_config(config=config)

    @property
    def name(self):
        return self.__class__.__name__

    def represent(self, image, **kwargs):
        logger.debug('Generating representation of given image')
        rep = self._represent(image, **kwargs)
        assert isinstance(rep, np.ndarray)
        return np.squeeze(rep)

    @abc.abstractproperty
    def type(self):
        pass

    @abc.abstractmethod
    def _represent(self, image, **kwargs):
        pass


class LightCNNFaceDescriptor(FaceDescriptor):
    """
    Light CNN face descriptor. Uses Caffe, CUDA and CuDNN or just CPU.
    https://github.com/AlfredXiangWu/face_verification_experiment
    """

    type = FaceDescriptorType.LIGHTCNN

    def __init__(self, deep_net=None, descriptor_layer=None, image_dim=None, config=None):
        super(LightCNNFaceDescriptor, self).__init__(config=config)
        self._descriptor_layer = self.config.data['faceDescriptor.lightcnn.layer'] if descriptor_layer is None else descriptor_layer
        self._load_deep_net(deep_net=deep_net)
        self._image_dim = self.config.data['faceDescriptor.lightcnn.imgDim'] if image_dim is None else image_dim

    @property
    def image_dim(self):
        return self._image_dim

    def _load_deep_net(self, deep_net=None):
        logger.debug('Loading deep net')
        if deep_net is not None:
            logger.debug('from passed argument')
            self._deep_net = deep_net
        else:
            model_path_proto = self.config.data['faceDescriptor.lightcnn.modelDir'] + '/' + self.config.data['faceDescriptor.lightcnn.networkModelProto']
            model_path_weights = self.config.data['faceDescriptor.lightcnn.modelDir'] + '/' + self.config.data['faceDescriptor.lightcnn.networkModelWeights']
            logger.debug('from default model path {}'.format(model_path_proto))
            if self.config.data['faceDescriptor.lightcnn.executeOnGPU']:
                logger.warning('Requested to run Caffe on GPU, if caffe was built without CUDA support on it will fail.')
                caffe.set_mode_cpu()
            else:
                logger.warning('Requested to run Caffe on CPU, if caffe was built with CUDA support on it will run on GPU not CPU. Known issue with caffe.')
                caffe.set_mode_cpu()
            self._deep_net = caffe.Net(model_path_proto, caffe.TEST, weights=model_path_weights)

    def _represent(self, image, **kwargs):
        if len(image.shape) > 2:
            logger.debug('Image is not grayscale, converting to grayscale')
            image = image.copy()
            image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

        # Preprocessing: Normalize the image
        image = image / 255.0
        image = cv2.resize(image, (self.image_dim, self.image_dim), interpolation=cv2.INTER_AREA)
        tensor = image[np.newaxis, np.newaxis, :, :]
        return self._forward_pass(tensor)

    def _forward_pass(self, tensor):
        self._deep_net.blobs['data'].reshape(*tensor.shape)
        self._deep_net.blobs['data'].data[...] = tensor
        _ = self._deep_net.forward()
        return np.copy(self._deep_net.blobs[self._descriptor_layer].data[0])


class FaceNetDescriptor(FaceDescriptor):
    """
    FaceNet face descriptor. Uses Tensorflow, CUDA and CuDNN.
    """

    type = FaceDescriptorType.FACENET

    def __init__(self, model_dir=None, session=None, config=None):
        super(FaceNetDescriptor, self).__init__(config=config)
        self._load_deep_net(model_dir=model_dir, session=session)

    def __del__(self):
        logger.debug('Closing Tensorflow graph')
        self._session.close()

    def _load_deep_net(self, model_dir=None, session=None):
        if session is not None:
            logger.debug('Using passed session')
            self._session = session
            self._session.__enter__()
        else:
            logger.debug('Loading Tensorflow graph')
            if model_dir is None:
                model_dir = self.config.data['faceDescriptor.facenet.modelDir']
                logger.debug('from default model directory path {}'.format(model_dir))
            else:
                logger.debug('from passed model directory path {}'.format(model_dir))
            self._graph = tf.Graph().as_default()
            self._graph.__enter__()
            self._session = tf.Session()
            self._session.__enter__()
            meta_file, ckpt_file = self._load_model_files(model_dir)
            saver = tf.train.import_meta_graph(os.path.join(model_dir, meta_file))
            saver.restore(tf.get_default_session(), os.path.join(model_dir, ckpt_file))

    def _load_model_files(self, model_dir):
        files = os.listdir(model_dir)
        meta_files = [s for s in files if s.endswith('.meta')]
        if len(meta_files) == 0:
            raise ValueError('No meta file found in the model directory ({})'.format(model_dir))
        elif len(meta_files) > 1:
            raise ValueError('There should not be more than one meta file in the model directory ({})'.format(model_dir))
        meta_file = meta_files[0]
        meta_files = [s for s in files if '.ckpt' in s]
        max_step = -1
        ckpt_file = None
        for f in meta_files:
            step_str = re.match(r'(^model-[\w\- ]+.ckpt-(\d+))', f)
            if step_str is not None and len(step_str.groups()) >= 2:
                step = int(step_str.groups()[1])
                if step > max_step:
                    max_step = step
                    ckpt_file = step_str.groups()[0]
        return meta_file, ckpt_file

    def _represent(self, image, images_placeholder=None, embeddings=None, phase_train_placeholder=None, **kwargs):
        if images_placeholder is None:
            images_placeholder = tf.get_default_graph().get_tensor_by_name('input:0')
        if embeddings is None:
            embeddings = tf.get_default_graph().get_tensor_by_name('embeddings:0')
        if phase_train_placeholder is None:
            phase_train_placeholder = tf.get_default_graph().get_tensor_by_name('phase_train:0')
        image_rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        image_rgb = cv2.resize(image_rgb, (images_placeholder.get_shape()[2], images_placeholder.get_shape()[1]), interpolation=cv2.INTER_AREA)
        image_rgb = prewhiten(image_rgb)
        images = np.stack([image_rgb])
        feed_dict = {images_placeholder: images, phase_train_placeholder: False}
        reps = self._session.run(embeddings, feed_dict=feed_dict)
        return reps
