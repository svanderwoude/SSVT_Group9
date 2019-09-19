import logging

import cv2
import numpy as np

logger = logging.getLogger(__name__)

try:
    import dlib
except ImportError as e:
    logger.error('Unable to import module: {}'.format(e))


def is_sharp_enough(image, threshold=0.0):
    img = image.copy()
    if len(img.shape) > 2:
        gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    else:
        gray_image = img
    s = gray_image.shape[0] * gray_image.shape[1]
    lap = cv2.Laplacian(gray_image, cv2.CV_64F)
    sharpness = (np.sum(np.abs(lap)) / float(s))
    return sharpness >= threshold


def load_cv_image(filepath, rgb=False):
    """Function to load an image using openCV.

    :param filepath: Path of the image to be loaded.
    :param rgb: If the image should be converted to RGB (from BGR).
    :return: An opencv image.
    """
    img = cv2.imread(filepath, cv2.IMREAD_COLOR)
    if rgb:
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    return img


def scale_image(image, scale_factor=1.0):
    """Rescale an image while retaining original aspect ratio.

    :param image: An opencv image.
    :param scale_factor: Scaling factor (0.0, 1.0]
    :return: An opencv image.
    """
    return cv2.resize(image, (0, 0), fx=scale_factor, fy=scale_factor)


def scale_rect(rect, scale_factor=1.0):
    """Simple function to rescale a bounding box.

    :param rect: A dlib.rectangle.
    :param scale_factor: Scaling factor (0.0, 1.0]
    :return: A dlib.rectangle.
    """
    return dlib.rectangle(int(scale_factor * rect.left()),
                          int(scale_factor * rect.top()),
                          int(scale_factor * rect.right()),
                          int(scale_factor * rect.bottom()))


def crop_image(image, rect):
    """Simple function to crop an image using a dlib rectangle.

    :param image: An opencv image.
    :param rect: A dlib.rectangle.
    :return: An opencv image.
    """
    return image[rect.top():rect.bottom(), rect.left():rect.right()]


def is_big_enough(rect, threshold=0):
    rect_size = rect.area()
    return rect_size >= threshold


def is_big_enough_relative(rect, image, threshold=0.0):
    image_size = image.shape[0] * image.shape[1]
    rect_size = rect.area()
    ratio = rect_size / float(image_size)
    return ratio >= threshold
