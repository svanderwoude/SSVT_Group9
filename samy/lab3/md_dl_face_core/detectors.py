import abc
import collections
import logging
from enum import Enum

import cv2
import numpy as np

from md_dl_face_core.mtcnn import create_mtcnn, detect_face
from md_dl_face_core.utils import scale_image, scale_rect, crop_image
from md_foundation.config.configuration_manager import ConfigurationMixin
from md_foundation.utils.common import get_camera_matrix, get_scale_factor, Pose

logger = logging.getLogger(__name__)


try:
    import dlib
    import tensorflow as tf
except ImportError as e:
    logger.error('Unable to import module: {}'.format(e))


DIST_COEFFS = np.zeros((4, 1))      # Assuming no lens distortion for simplicity

DL_MODEL_2D_INDS = [
    27,
    36,
    45,
    0,
    16,
    30,
    66,
    8
]


DL_MODEL_3D = np.array([
    (0., 0., 0.),           # nose between eyes
    (-65.5, -5., -20.),     # eye outer right
    (65.5, -5., -20.),      # eye outer left
    (-77.5, -6., -100.),    # ear right
    (77.5, -6., -100.),     # ear left
    (0., -48.0, 21.),       # nose tip
    (0., -75., 10.),        # mouth center
    (0., -133., 0.),        # chin
], dtype=np.float32)


DLIB_TEMPLATE = np.float32([
    (0.0792396913815, 0.339223741112), (0.0829219487236, 0.456955367943),
    (0.0967927109165, 0.575648016728), (0.122141515615, 0.691921601066),
    (0.168687863544, 0.800341263616), (0.239789390707, 0.895732504778),
    (0.325662452515, 0.977068762493), (0.422318282013, 1.04329000149),
    (0.531777802068, 1.06080371126), (0.641296298053, 1.03981924107),
    (0.738105872266, 0.972268833998), (0.824444363295, 0.889624082279),
    (0.894792677532, 0.792494155836), (0.939395486253, 0.681546643421),
    (0.96111933829, 0.562238253072), (0.970579841181, 0.441758925744),
    (0.971193274221, 0.322118743967), (0.163846223133, 0.249151738053),
    (0.21780354657, 0.204255863861), (0.291299351124, 0.192367318323),
    (0.367460241458, 0.203582210627), (0.4392945113, 0.233135599851),
    (0.586445962425, 0.228141644834), (0.660152671635, 0.195923841854),
    (0.737466449096, 0.182360984545), (0.813236546239, 0.192828009114),
    (0.8707571886, 0.235293377042), (0.51534533827, 0.31863546193),
    (0.516221448289, 0.396200446263), (0.517118861835, 0.473797687758),
    (0.51816430343, 0.553157797772), (0.433701156035, 0.604054457668),
    (0.475501237769, 0.62076344024), (0.520712933176, 0.634268222208),
    (0.565874114041, 0.618796581487), (0.607054002672, 0.60157671656),
    (0.252418718401, 0.331052263829), (0.298663015648, 0.302646354002),
    (0.355749724218, 0.303020650651), (0.403718978315, 0.33867711083),
    (0.352507175597, 0.349987615384), (0.296791759886, 0.350478978225),
    (0.631326076346, 0.334136672344), (0.679073381078, 0.29645404267),
    (0.73597236153, 0.294721285802), (0.782865376271, 0.321305281656),
    (0.740312274764, 0.341849376713), (0.68499850091, 0.343734332172),
    (0.353167761422, 0.746189164237), (0.414587777921, 0.719053835073),
    (0.477677654595, 0.706835892494), (0.522732900812, 0.717092275768),
    (0.569832064287, 0.705414478982), (0.635195811927, 0.71565572516),
    (0.69951672331, 0.739419187253), (0.639447159575, 0.805236879972),
    (0.576410514055, 0.835436670169), (0.525398405766, 0.841706377792),
    (0.47641545769, 0.837505914975), (0.41379548902, 0.810045601727),
    (0.380084785646, 0.749979603086), (0.477955996282, 0.74513234612),
    (0.523389793327, 0.748924302636), (0.571057789237, 0.74332894691),
    (0.672409137852, 0.744177032192), (0.572539621444, 0.776609286626),
    (0.5240106503, 0.783370783245), (0.477561227414, 0.778476346951)])

DLIB_TPL_MIN, DLIB_TPL_MAX = np.min(DLIB_TEMPLATE, axis=0), np.max(DLIB_TEMPLATE, axis=0)
DLIB_MINMAX_TEMPLATE = (DLIB_TEMPLATE - DLIB_TPL_MIN) / (DLIB_TPL_MAX - DLIB_TPL_MIN)


class FaceDetectorType(Enum):
    DLIB = 1
    DLIBROTATIONALIGN = 2
    MTCNN = 3


class FaceDetector(abc.ABC, ConfigurationMixin):
    """
    Abstract base wrapper class for different types of face detectors.
    """

    def __init__(self, config=None):
        logger.debug('Loading {}'.format(self.name))
        self.load_config(config=config)

    @property
    def name(self):
        return self.__class__.__name__

    def detect(self, image, **kwargs):
        logger.debug('Detecting face(s) in image')
        scale_factor = get_scale_factor(image, self.config.data.get('faceDetector.image.maxHeight', 1080), self.config.data.get('faceDetector.image.maxWidth', 1920))
        logger.debug('Scaling image using scale factor {:.3f}'.format(scale_factor))
        image_scaled = scale_image(image, scale_factor=scale_factor)
        rects = self._detect(image_scaled, **kwargs)
        assert isinstance(rects, list)
        rects = [scale_rect(r, scale_factor=1./scale_factor) for r in rects]
        logger.debug('Found {} bounding box(es)'.format(len(rects)))
        return rects

    def align(self, image, rect, **kwargs):
        if self.config.data['faceDetector.align']:
            logger.debug('Aligning face in image')
            face = self._align(image, rect, **kwargs)
        else:
            logger.debug('Cropping face in image')
            face = crop_image(image, rect)
        return face

    def pose(self, image, rect, **kwargs):
        logger.debug('Estimating pose of face in image')
        return self._pose(image, rect, **kwargs)

    def landmarks(self, image, rect, **kwargs):
        logger.debug('Finding landmarks in face')
        return self._landmarks(image, rect, **kwargs)

    @abc.abstractproperty
    def type(self):
        pass

    @abc.abstractmethod
    def _detect(self, image, **kwargs):
        pass

    @abc.abstractmethod
    def _align(self, image, rect, **kwargs):
        pass

    @abc.abstractmethod
    def _pose(self, image, rect, **kwargs):
        pass

    @abc.abstractmethod
    def _landmarks(self, image, rect, **kwargs):
        pass


class DlibFaceDetector(FaceDetector):
    """
    Dlib face detector implementation.
    """

    type = FaceDetectorType.DLIB

    INNER_EYES_AND_BOTTOM_LIP = [39, 42, 57]
    OUTER_EYES_AND_NOSE = [36, 45, 33]

    def __init__(self, detector=None, predictor=None, config=None):
        super(DlibFaceDetector, self).__init__(config=config)
        self._load_detector(detector=detector)
        self._load_predictor(predictor=predictor)

    def _load_detector(self, detector=None):
        logger.debug('Loading detector...')
        if detector is not None:
            logger.debug('from passed argument')
            self._detector = detector
        else:
            logger.debug('from default detector')
            self._detector = dlib.get_frontal_face_detector()

    def _load_predictor(self, predictor=None):
        logger.debug('Loading predictor')
        if predictor is not None:
            logger.debug('from passed argument')
            self._predictor = predictor
        else:
            predictor_path = self.config.data['faceDetector.dlib.modelDir'] + '/' + self.config.data['faceDetector.dlib.facePredictor']
            logger.debug('from default filepath {}'.format(predictor_path))
            self._predictor = dlib.shape_predictor(predictor_path)

    def _detect(self, image, **kwargs):
        image_gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        try:
            rects = self._detector(image_gray, 1)
            rects = list(rects)
        except Exception as e:
            logger.exception(e)
            rects = []
        return rects

    def _align(self, image, rect, landmarks=None, landmark_indices=None, image_dim=None, **kwargs):
        if landmarks is None:
            landmarks = self._landmarks(image, rect)
        if landmark_indices is None:
            landmark_indices = type(self).OUTER_EYES_AND_NOSE
        if image_dim is None:
            desired_face_width = self.config.data['faceDetector.align.desiredFaceWidth']
            desired_face_height = self.config.data['faceDetector.align.desiredFaceHeight']
            image_dim = max(desired_face_width, desired_face_height)
            if desired_face_width != desired_face_height:
                logger.warning('desiredFaceWidth ({width}) != desiredFaceHeight ({height}). Using maximum value ({maximum})!'.format(width=desired_face_width, height=desired_face_height, maximum=image_dim))
        np_landmarks = np.float32(landmarks)
        np_landmark_indices = np.array(landmark_indices)
        h_ = cv2.getAffineTransform(np_landmarks[np_landmark_indices], image_dim * DLIB_MINMAX_TEMPLATE[np_landmark_indices])
        thumbnail = cv2.warpAffine(image, h_, (image_dim, image_dim))
        return thumbnail

    def _pose(self, image, rect, landmarks=None, **kwargs):
        if landmarks is None:
            landmarks = self._landmarks(image, rect)
        landmarks = np.float32(landmarks)
        cmat = get_camera_matrix(image.shape[0], image.shape[1])
        _, rvec, tvec, _ = cv2.solvePnPRansac(objectPoints=DL_MODEL_3D, imagePoints=landmarks[DL_MODEL_2D_INDS], cameraMatrix=cmat,
                                          distCoeffs=DIST_COEFFS, reprojectionError=50)
        rmat, _ = cv2.Rodrigues(rvec)
        pmat = np.hstack((rmat, tvec))
        euler_angles = cv2.decomposeProjectionMatrix(pmat)[-1]
        euler_angles[euler_angles > 90] = euler_angles[euler_angles > 90] - 180.    # adjustment for degree issue; only really necessary for pitch
        euler_angles[euler_angles < -90] = euler_angles[euler_angles < -90] + 180.  # adjustment for degree issue; only really necessary for pitch
        return Pose(pitch=euler_angles[0][0], yaw=euler_angles[1][0], roll=euler_angles[2][0])

    def _landmarks(self, image, rect, **kwargs):
        points = self._predictor(image, rect)
        landmarks = list(map(lambda p: (p.x, p.y), points.parts()))
        return landmarks


class FaceRotationAlignMixin:

    def _apply_affine_transformation(self, image, rot_matrix, desired_face_height, desired_face_width):
        (w, h) = (desired_face_width, desired_face_height)
        thumbnail = cv2.warpAffine(image, rot_matrix, (w, h), flags=cv2.INTER_CUBIC)
        return thumbnail

    def _update_translation_component_rotation_matrix(self, rot_matrix, desired_face_height, desired_face_width, desired_left_eye, eyes_center):
        t_x = desired_face_width * 0.5
        t_y = desired_face_height * desired_left_eye[1]
        rot_matrix[0, 2] += (t_x - eyes_center[0])
        rot_matrix[1, 2] += (t_y - eyes_center[1])

    def _get_left_right_eye_centroid(self, shape):
        return (shape[1], shape[6]), (shape[0], shape[5])

    def _get_point_between_eyes(self, left_eye_center, right_eye_center):
        eyes_center = ((left_eye_center[0] + right_eye_center[0]) // 2,
                       (left_eye_center[1] + right_eye_center[1]) // 2)
        return eyes_center

    def _compute_angle_between_eye_centroids(self, left_eye_centroid, right_eye_centroid):
        d_y = right_eye_centroid[1] - left_eye_centroid[1]
        d_x = right_eye_centroid[0] - left_eye_centroid[0]
        angle = np.degrees(np.arctan2(d_y, d_x)) - 180
        return angle

    def _compute_scale(self, left_eye_centeroid, right_eye_centroid, desired_left_eye, desired_face_width):
        desired_right_eye_x = 1.0 - desired_left_eye[0]
        d_y = right_eye_centroid[1] - left_eye_centeroid[1]
        d_x = right_eye_centroid[0] - left_eye_centeroid[0]
        dist = np.sqrt((d_x ** 2) + (d_y ** 2))
        desired_dist = (desired_right_eye_x - desired_left_eye[0])
        desired_dist *= desired_face_width
        scale = desired_dist / dist
        return scale


class DlibFaceDetectorRotateAlign(DlibFaceDetector, FaceRotationAlignMixin):
    """
    Dlib face detector implementation with alternate alignment method.
    """

    type = FaceDetectorType.DLIBROTATIONALIGN

    FACIAL_LANDMARKS_IDXS = collections.OrderedDict([
        ('mouth', (48, 68)),
        ('right_eyebrow', (17, 22)),
        ('left_eyebrow', (22, 27)),
        ('right_eye', (36, 42)),
        ('left_eye', (42, 48)),
        ('nose', (27, 36)),
        ('jaw', (0, 17))
    ])

    def _align(self, image, rect, landmarks=None, landmark_indices=None, image_dim=None, desired_left_eye=None, desired_face_width=None, desired_face_height=None, **kwargs):
        if desired_left_eye is None:
            desired_left_eye = (self.config.data['faceDetector.align.rotation.desiredLeftEyeY'], self.config.data['faceDetector.align.rotation.desiredLeftEyeX'])
        if desired_face_width is None:
            desired_face_width = self.config.data['faceDetector.align.desiredFaceWidth']
        if desired_face_height is None:
            desired_face_height = self.config.data['faceDetector.align.desiredFaceHeight']
        if landmarks is None:
            landmarks = self._landmarks(image, rect)

        left_eye_centroid = self._get_eye_centroid(landmarks, 'left_eye')
        right_eye_centroid = self._get_eye_centroid(landmarks, 'right_eye')

        angle = self._compute_angle_between_eye_centroids(left_eye_centroid, right_eye_centroid)
        scale = self._compute_scale(left_eye_centroid, right_eye_centroid, desired_left_eye, desired_face_width)
        eyes_center = self._get_point_between_eyes(left_eye_center=left_eye_centroid, right_eye_center=right_eye_centroid)

        rot_matrix = cv2.getRotationMatrix2D(eyes_center, angle, scale)

        self._update_translation_component_rotation_matrix(rot_matrix, desired_face_height, desired_face_width, desired_left_eye, eyes_center)

        return self._apply_affine_transformation(image, rot_matrix, desired_face_height, desired_face_width)

    def _get_eye_centroid(self, shape, which_eye_str):
        shape = self._shape_to_np(shape)

        (lStart, lEnd) = self.FACIAL_LANDMARKS_IDXS[which_eye_str]
        eye_pts = shape[lStart:lEnd]

        eye_centroid = eye_pts.mean(axis=0).astype('int')
        return eye_centroid

    def _shape_to_np(self, shape, dtype='int'):
        coords = np.zeros((68, 2), dtype=dtype)
        for i in range(68):
            coords[i] = shape[i]
        return coords


class MTCNNFaceDetector(FaceDetector, FaceRotationAlignMixin):
    """
    MTCNN face detector implementation. Uses three networks in pipeline. Copied code under MIT license.
    """

    type = FaceDetectorType.MTCNN

    mtcnn_landmarks = collections.namedtuple('mtcnn_landmarks', 'eye_left eye_right nose mouth_left mouth_right')

    def __init__(self, gpu_memory_fraction=None, pnet=None, rnet=None, onet=None, config=None):
        super(MTCNNFaceDetector, self).__init__(config=config)
        logger.warning("MTCNN doesn't perform any pose estimation.")
        if gpu_memory_fraction is None:
            self._gpu_memory_fraction = self.config.data['faceDetector.mtcnn.gpuMemoryFraction']
        else:
            self._gpu_memory_fraction = gpu_memory_fraction
        self._load_mtcnn(pnet=pnet, rnet=rnet, onet=onet)

    def _load_mtcnn(self, pnet=None, rnet=None, onet=None):
        logger.debug('Loading MTCNN Networks')
        if not all([pnet, rnet, onet]):
            model_dir = self.config.data['faceDetector.mtcnn.modelDir']
            logger.debug('from default model directory path {}'.format(model_dir))
            with tf.Graph().as_default():
                gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=self._gpu_memory_fraction)
                sess = tf.Session(config=tf.ConfigProto(gpu_options=gpu_options, log_device_placement=False))
                with sess.as_default():
                    pnet, rnet, onet = create_mtcnn(sess, model_dir)
        else:
            logger.debug('from passed arguments')
        self._pnet = pnet
        self._rnet = rnet
        self._onet = onet

    @property
    def gpu_memory_fraction(self):
        return self._gpu_memory_fraction

    def _detect(self, image, min_face_size=None, detect_thresholds=None, margin=None, **kwargs):
        if min_face_size is None:
            min_face_size = self.config.data['faceDetector.mtcnn.minFaceSize']
        if detect_thresholds is None or len(detect_thresholds) != 3:
            detect_thresholds = self.config.data['faceDetector.mtcnn.detectThresholds']
        if margin is None:
            margin = self.config.data['faceDetector.mtcnn.faceMargin']
        image_rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        bbs, _ = detect_face(image_rgb, min_face_size, self._pnet, self._rnet, self._onet, threshold=detect_thresholds)
        rects = self._parse_bbs(bbs, margin=margin, image_height=image.shape[0], image_width=image.shape[1])
        return rects

    def _parse_bbs(self, bbs, margin, image_height, image_width):
        rects = []
        for bb in bbs:
            bb_squeezed = np.squeeze(bb[0:4])
            left = int(np.maximum(bb_squeezed[0] - (margin / 2.0), 0))
            top = int(np.maximum(bb_squeezed[1] - (margin / 2.0), 0))
            right = int(np.minimum(bb_squeezed[2] + (margin / 2.0), image_width))
            bottom = int(np.minimum(bb_squeezed[3] + (margin / 2.0), image_height))
            rect = dlib.rectangle(left, top, right, bottom)
            rects.append(rect)
        return rects

    def _align(self, image, rect, landmarks=None, margin=None, image_dim=None, desired_left_eye=None, desired_face_width=None, desired_face_height=None, **kwargs):
        if desired_left_eye is None:
            desired_left_eye = (self.config.data['faceDetector.align.rotation.desiredLeftEyeY'], self.config.data['faceDetector.align.rotation.desiredLeftEyeX'])
        if desired_face_width is None:
            desired_face_width = self.config.data['faceDetector.align.desiredFaceWidth']
        if desired_face_height is None:
            desired_face_height = self.config.data['faceDetector.align.desiredFaceHeight']
        if landmarks is None:
            landmarks = self._landmarks(image, rect)
            if landmarks is None:
                logger.error('Landmark detection failed! Skipping alignment!')
                return crop_image(image, rect)

        left_eye_centeroid, right_eye_centroid = self._get_left_right_eye_centroid(landmarks)

        angle = self._compute_angle_between_eye_centroids(left_eye_centeroid, right_eye_centroid)
        scale = self._compute_scale(left_eye_centeroid, right_eye_centroid, desired_left_eye, desired_face_width)
        eyes_center = self._get_point_between_eyes(left_eye_center=left_eye_centeroid, right_eye_center=right_eye_centroid)

        rot_matrix = cv2.getRotationMatrix2D(eyes_center, angle, scale)

        self._update_translation_component_rotation_matrix(rot_matrix, desired_face_height, desired_face_width, desired_left_eye, eyes_center)

        cropped = self._apply_affine_transformation(image, rot_matrix, desired_face_height, desired_face_width)
        return cropped

    def _pose(self, image, rect, **kwargs):
        raise NotImplementedError('Face pose estimation has not yet been implemented for MTCNN.')

    def _landmarks(self, image, rect, min_face_size=None, detect_thresholds=None, margin=None, **kwargs):
        if min_face_size is None:
            min_face_size = self.config.data['faceDetector.mtcnn.minFaceSize']
        if detect_thresholds is None or len(detect_thresholds) != 3:
            detect_thresholds = self.config.data['faceDetector.mtcnn.detectThresholds']
        image_rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        image_rgb_crop_to_box = crop_image(image_rgb, rect)
        _, points = detect_face(image_rgb_crop_to_box, min_face_size, self._pnet, self._rnet, self._onet, threshold=detect_thresholds)

        # if there is a face detected
        if points.size is not 0:
            # update point coords due to cropped image
            # left eye
            points[0] += rect.left()
            points[5] += rect.top()
            # right eye
            points[1] += rect.left()
            points[6] += rect.top()
            # nose
            points[2] += rect.left()
            points[7] += rect.top()
            # mouthl
            points[3] += rect.left()
            points[8] += rect.top()
            # mouthr
            points[4] += rect.left()
            points[9] += rect.top()

            return points.astype('int')
        else:
            return None
