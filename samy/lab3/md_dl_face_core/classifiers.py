import abc
import logging
from collections import Counter

import numpy as np
from scipy.spatial.distance import cdist
from sklearn.metrics import fbeta_score
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import LabelEncoder

from md_foundation.config.configuration_manager import ConfigurationMixin
from md_foundation.utils.common import get_euclidean_distance

logger = logging.getLogger(__name__)


class FaceClassifier(abc.ABC, ConfigurationMixin):
    """
    Abstract base wrapper class for face classifier. Uses scikit-learn structure (fit, predict, predict_proba, etc.)
    """
    def __init__(self, config=None):
        logger.debug('Loading {}'.format(self.name))
        self.load_config(config=config)
        self._label_encoder = None
        self.nfeatures = None
        self.fitted = False

    @property
    def name(self):
        return self.__class__.__name__

    @property
    def classes(self):
        try:
            return list(self._label_encoder.classes_)
        except AttributeError:
            return []

    labels = classes

    def transform_label(self, label):
        return self._label_encoder.transform([label])[0]

    def transform_label_num(self, label_num):
        return self._label_encoder.inverse_transform([label_num])[0]

    def fit_label_encoder(self, labels):
        labels = np.array(labels)
        self._label_encoder = LabelEncoder().fit(labels)

    def fit(self, reps, labels, **kwargs):
        if self._label_encoder is None:
            raise ValueError('LabelEncoder has not been fitted yet! Call fit_label_encoder() before fit().')
        logger.debug('Fitting classifier')
        labels_num = self._label_encoder.transform(labels)
        reps = np.array(reps)
        self._fit(reps, labels_num, **kwargs)
        self.nfeatures = reps.shape[1]
        self.fitted = True

    def predict(self, rep, **kwargs):
        logger.debug('Predicting representation')
        assert self.fitted, 'Classifier has not been fitted!'
        assert self.nfeatures == len(rep), 'Number of features in input ({}) do not match expected ({})!'.format(len(rep), self.nfeatures)
        label_num = self._predict(rep, **kwargs)
        return self._label_encoder.inverse_transform([label_num])[0]

    classify = predict

    def predict_proba(self, rep, **kwargs):
        logger.debug('Predicting representation (with confidence)')
        assert self.fitted, 'Classifier has not been fitted!'
        assert self.nfeatures == len(rep), 'Number of features in input ({}) do not match expected ({})!'.format(len(rep), self.nfeatures)
        probs = self._predict_proba(rep, **kwargs)
        assert isinstance(probs, np.ndarray)
        return np.squeeze(probs)

    classify_proba = predict_proba

    @abc.abstractmethod
    def _fit(self, reps, labels_num, **kwargs):
        pass

    @abc.abstractmethod
    def _predict(self, rep, **kwargs):
        pass

    @abc.abstractmethod
    def _predict_proba(self, rep, **kwargs):
        pass


class ScikitLearnFaceClassifier(FaceClassifier):
    """
    Face classifier wrapper class for scikit-learn classifiers. Shows warning if certain functions are not supported.
    """

    def __init__(self, classifier=None, config=None):
        super(ScikitLearnFaceClassifier, self).__init__(config=config)
        self._classifier = None
        self._load_classifier(classifier=classifier)
        for func in ['fit', 'predict', 'predict_proba', 'partial_fit']:
            if not hasattr(self._classifier, func):
                logger.warning("Wrapped classifier '{}' does not have an attribute called {}!".format(self._classifier.__class__.__name__, func))

    def _load_classifier(self, classifier=None):
        logger.debug('Loading wrapped classifier')
        if classifier is None:
            self._classifier = MLPClassifier(solver='adam', activation='relu', alpha=0.05, learning_rate_init=1e-3, hidden_layer_sizes=(100,), tol=1e-6, batch_size=1000, max_iter=1000)
        else:
            self._classifier = classifier
        logger.debug('of type {}'.format(self._classifier.__class__.__name__))

    @property
    def classifier(self):
        return self._classifier

    @classifier.setter
    def classifier(self, value):
        self._classifier = value

    def _fit(self, reps, labels_num, **kwargs):
        self._classifier.fit(reps, labels_num)

    def _predict(self, rep, **kwargs):
        rep_reshaped = rep.reshape(1, -1)
        return self._classifier.predict(rep_reshaped)[0]

    def _predict_proba(self, rep, **kwargs):
        rep_reshaped = rep.reshape(1, -1)
        return self._classifier.predict_proba(rep_reshaped)

    def partial_fit(self, reps, labels, classes=None):
        if self._label_encoder is None:
            raise ValueError('LabelEncoder has not been fitted yet! Call fit_label_encoder() before partial_fit().')
        labels_num = self._label_encoder.transform(labels)
        if classes is not None:
            classes = self._label_encoder.transform(classes)
        self._classifier.partial_fit(reps, labels_num, classes=classes)
        self.nfeatures = reps.shape[1]
        self.fitted = True


class DistanceFaceClassifier(FaceClassifier, abc.ABC):
    """
    Abstract base class for face classifiers that use simple (euclidean) distance checking for classification.
    """
    def __init__(self, distance_metric='euclidean', config=None):
        super(DistanceFaceClassifier, self).__init__(config=config)
        self._reps = None
        self._labels_num = None
        self._distance_metric = distance_metric

    @property
    def distance_metric(self):
        return self._distance_metric

    def get_distances(self, rep, inds=None):
        rep = rep.copy()
        rep = rep.reshape(1, -1)
        distance_metric = self.distance_metric
        if distance_metric.lower().startswith('rectified'):
            distance_metric = distance_metric.split('_')[-1]
            rep[rep < 0] = np.float32(0)
        if inds is None:
            inds = np.arange(len(self._reps))
        return cdist(self._reps[inds], rep, metric=distance_metric).reshape(-1)

    @abc.abstractmethod
    def _fit(self, reps, labels_num, **kwargs):
        pass

    @abc.abstractmethod
    def _predict(self, rep, **kwargs):
        pass

    @abc.abstractmethod
    def _predict_proba(self, rep, **kwargs):
        pass


class KNNDistanceFaceClassifier(DistanceFaceClassifier):
    """
    K Nearest Neighbour classifier. Can give softmax confidence values. Higher k values give more reliable confidence values..
    """

    def __init__(self, k=1, distance_metric='euclidean', config=None):
        super(KNNDistanceFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._k = k

    @property
    def k(self):
        return self._k

    def _fit(self, reps, labels_num, **kwargs):
        self._reps = reps.copy()
        self._labels_num = labels_num.copy()

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _predict_proba(self, rep, **kwargs):
        dists = self.get_distances(rep)
        closest_inds = np.argsort(dists)[:self._k]
        closest_labels = self._labels_num[closest_inds]
        unique, counts = np.unique(closest_labels, return_counts=True)  # majority vote
        confs = np.zeros(len(self.classes))
        for label_num, count in zip(unique, counts):
            confs[label_num] = count / float(np.sum(counts))
        return confs


class TopKDistanceClassifier(KNNDistanceFaceClassifier):
    """
    Very similar to K Nearest Neighbour classifier. Uses scipy's distance.cdist method that is super fast and
    can work with both cosine similarity and Euclidean distance.
    Can give softmax confidence values. Higher k values give more reliable confidence values..
    """
    def __init__(self, k=1, distance_metric='cosine', config=None):
        super(KNNDistanceFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._k = k

    def _predict_proba(self, rep, **kwargs):
        distances = self.get_distances(rep)
        sorted_indexes = np.argsort(distances)
        labels = self._labels_num[sorted_indexes[:self._k]]
        count_labels = Counter(labels)
        sumis = sum(count_labels.values())
        for k in count_labels.keys():
            count_labels[k] /= float(sumis)
        confs = np.zeros(len(self.classes))
        for k in count_labels.keys():
            confs[k] = count_labels[k]
        return confs


class StrongMatchFaceClassifier(DistanceFaceClassifier):
    """
    Face classifier that uses the strong matching approach from SIFT to calculate confidence values.
    """

    def __init__(self, k=10, ratio_threshold=2.0, distance_metric='euclidean', config=None):
        super(StrongMatchFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._k = k
        self._ratio_threshold = float(ratio_threshold)

    @property
    def k(self):
        return self._k

    @property
    def ratio_threshold(self):
        return self._ratio_threshold

    def _fit(self, reps, labels_num, **kwargs):
        self._reps = reps.copy()
        self._labels_num = labels_num.copy()
        if len(self._labels_num) < self._k:
            raise ValueError('Number of training instances is smaller than K parameter ({}<{})! Reduce K or increase amount of training instances!'.format(len(self._labels_num), self._k))

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _get_strong_match_confidence_by_dist(self, xd, yd):
        ratio = xd / float(yd)
        conf = 1.0 - ((ratio - (1.0 / self._ratio_threshold)) / (1.0 - (1.0 / self._ratio_threshold)))
        return min(max(conf, 0.0), 1.0)

    def _get_confidences(self, label_score_map):
        labels, scores = map(np.array, zip(*label_score_map))
        confidences = np.zeros(len(self.classes))
        unique_labels = np.unique(labels)
        for unique_label in unique_labels:
            label_scores = scores[labels == unique_label]
            label_score = np.mean(label_scores)
            confidences[unique_label] = label_score
        if np.sum(confidences) > 1.0:
            confidences /= np.sum(confidences)
        return confidences

    def _adjust_scores(self, label_score_map):
        labels, scores = map(np.array, zip(*label_score_map))
        unique_labels, unique_counts = np.unique(labels, return_counts=True)
        for unique_label, unique_count in zip(unique_labels, unique_counts):
            scores[labels == unique_label] *= unique_count
        rank_weights = np.linspace(1.0, 1 / float(self._k), num=self._k)
        scores *= rank_weights
        label_score_map = zip(labels, scores)
        return label_score_map

    def _get_strong_match_scores(self, label_dist_map_top_k):
        scores = np.zeros(self._k)
        labels = []
        for i1, (label1, distance1) in enumerate(label_dist_map_top_k):
            labels.append(label1)
            for i2, (label2, distance2) in enumerate(label_dist_map_top_k):
                if i1 == i2:
                    continue
                scores[i1] += 1.0 if label1 == label2 else self._get_strong_match_confidence_by_dist(distance1, distance2)
        scores /= float(self._k - 1)
        label_score_map = zip(labels, scores)
        return label_score_map

    def _predict_proba(self, rep, **kwargs):
        dists = self.get_distances(rep)
        label_dist_map = zip(self._labels_num, dists)
        label_dist_map_top_k = sorted(label_dist_map, key=lambda x: x[1])[:self._k]
        label_score_map = self._get_strong_match_scores(label_dist_map_top_k)
        label_score_map = self._adjust_scores(label_score_map)
        confidences = self._get_confidences(label_score_map)
        return confidences


class StrongMatchKNNFaceClassifier(DistanceFaceClassifier):
    """
    Face classifier that uses the strong matching approach from SIFT to calculate confidence values.
    Due to the nature of the approach only a confidence value for the closest point can be calculated.
    """

    def __init__(self, k=10, ratio_threshold=2.0, count_threshold=0.6, distance_metric='euclidean', config=None):
        super(StrongMatchKNNFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._k = k
        self._ratio_threshold = float(ratio_threshold)
        self._count_threshold = float(count_threshold)

    @property
    def k(self):
        return self._k

    @property
    def ratio_threshold(self):
        return self._ratio_threshold

    @property
    def count_threshold(self):
        return self._count_threshold

    def _fit(self, reps, labels_num, **kwargs):
        self._reps = reps.copy()
        self._labels_num = labels_num.copy()
        if len(self._labels_num) < self._k:
            raise ValueError('Number of training instances is smaller than K parameter ({}<{})! Reduce K or increase amount of training instances!'.format(len(self._labels_num), self._k))

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _is_strong_match_by_dist(self, xd, yd):
        return yd >= (xd * self._ratio_threshold)

    def _get_confidence(self, label_dist_map_top_k, top_label):
        strong = 0
        total = 0
        for label1, distance1 in label_dist_map_top_k:
            if label1 != top_label:
                continue
            for label2, distance2 in label_dist_map_top_k:
                if label1 == label2:
                    continue
                strong += self._is_strong_match_by_dist(distance1, distance2)
                total += 1
        return strong / float(total)

    def _get_confidences(self, label_dist_map_top_k):
        confidences = np.zeros(len(self.classes))
        labels, _ = zip(*label_dist_map_top_k)
        unique_labels, unique_counts = np.unique(labels, return_counts=True)
        top_label = unique_labels[np.argmax(unique_counts)]
        if np.max(unique_counts) >= (self._k * self._count_threshold):
            confidences[top_label] = 1.0
        else:
            confidences[top_label] = self._get_confidence(label_dist_map_top_k, top_label)
        return confidences

    def _predict_proba(self, rep, **kwargs):
        dists = self.get_distances(rep)
        label_dist_map = zip(self._labels_num, dists)
        label_dist_map_top_k = sorted(label_dist_map, key=lambda x: x[1])[:self._k]
        confidences = self._get_confidences(label_dist_map_top_k)
        return confidences


class DistanceHitFaceClassifier(DistanceFaceClassifier):
    """
    Face classifier that uses two thresholds: an absolute distance threshold and a number of hits thresholds.
    The number of hits means the number of points per class that exceed the distance threshold.
    Separate confidence values are calculated for both "steps" and are used to calculate a weighted final list of confidences.
    """

    def __init__(self, distance_threshold=1.0, hit_threshold=2, alpha=0.7, distance_metric='euclidean', config=None):
        super(DistanceHitFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._distance_threshold = float(distance_threshold)
        self._hit_threshold = int(hit_threshold)
        self._alpha = float(alpha)

    @property
    def distance_threshold(self):
        return self._distance_threshold

    @property
    def hit_threshold(self):
        return self._hit_threshold

    @property
    def alpha(self):
        return self._alpha

    def _fit(self, reps, labels_num, **kwargs):
        self._reps = reps.copy()
        self._labels_num = labels_num.copy()

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _predict_proba(self, rep, **kwargs):
        hits = np.zeros(len(self.classes))
        hits_max = np.zeros(len(self.classes))
        dists = np.zeros(len(self.classes))
        distances = self.get_distances(rep)
        for label_num, dist in zip(self._labels_num, distances):
            dists[label_num] += dist
            hits_max[label_num] += 1
            if dist < self._distance_threshold:
                hits[label_num] += 1
        dists[hits > 0] /= hits[hits > 0]       # compute mean per class where hit > 0
        dist_confs = 1.0 - (dists / self._distance_threshold)   # convert to 0.0 - 1.0 range
        dist_confs[dist_confs < 0] = 0.0        # classes where hit < 1 are set to 0
        hits -= (self._hit_threshold - 1)
        hits[hits < 0] = 0.0
        hits_max -= (self._hit_threshold - 1)
        hit_confs = np.zeros(len(self.classes))
        hit_confs[hits_max > 0] = hits[hits_max > 0] / hits_max[hits_max > 0]
        confs = (self._alpha * dist_confs) + ((1 - self._alpha) * hit_confs)
        return confs


class ClusterThresholdFaceClassifier(FaceClassifier):
    """
    Face classifier that calculates center of a face cluster and computes a cluster-specific threshold for confidence calculation.
    Clusters with a distance to the query point greater than (or equal to) the clusters threshold have a 0 percent confidence for prediction.
    """

    def __init__(self, config=None):
        super(ClusterThresholdFaceClassifier, self).__init__(config=config)
        self._clusters = None

    def _threshold_func(self, cluster, nof_stds=15):
        center = np.mean(cluster, axis=0)
        dists = []
        for point in cluster:
            d = get_euclidean_distance(center, point)
            dists.append(d)
        std_val = np.std(dists)
        return std_val * float(nof_stds)

    def _fit(self, reps, labels_num, threshold_func=None):
        if not callable(threshold_func):
            threshold_func = self._threshold_func
        clusters = [{'center': [], 'threshold': None} for _ in range(len(self.classes))]
        for label_num, rep in zip(labels_num, reps):
            clusters[label_num]['center'].append(rep)
        for label_num in range(len(self.classes)):
            clusters[label_num]['threshold'] = threshold_func(clusters[label_num]['center'])
            clusters[label_num]['center'] = np.mean(clusters[label_num]['center'], axis=0)
        self._clusters = clusters

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _predict_proba(self, rep, **kwargs):
        dists = np.zeros(len(self.classes))
        thresholds = np.zeros(len(self.classes))
        for label_num, cluster in enumerate(self._clusters):
            dist = get_euclidean_distance(rep, cluster['center'])
            dists[label_num] = dist
            thresholds[label_num] = cluster['threshold']
        confs = 1.0 - (dists / thresholds)
        confs[confs < 0] = 0.0
        return confs


class ClusterKNNFaceClassifier(DistanceFaceClassifier):

    def __init__(self, alpha=0.5, fbeta=1.0, k=10, gamma=2, distance_metric='cosine', config=None):
        super(ClusterKNNFaceClassifier, self).__init__(distance_metric=distance_metric, config=config)
        self._cluster_centers = None
        self._cluster_thresholds = None
        self._alpha = float(alpha)
        self._beta = float(fbeta)
        self._gamma = int(gamma)
        self._k = int(k)

    def _fit(self, reps, labels_num, **kwargs):
        self._reps = reps.copy()
        self._labels_num = labels_num.copy()
        self._fit_clusters()

    def _fit_clusters(self):
        centers = []
        thresholds = []
        for cls in self.classes:
            cls_num = self._label_encoder.transform([cls])[0]
            cls_inds = np.where(self._labels_num == cls_num)[0]
            cls_center = np.median(self._reps[cls_inds], axis=0)
            cls_center_dists = self.get_distances(cls_center, inds=cls_inds)
            cls_center_dists_sorted_inds = np.argsort(cls_center_dists)
            cls_dist_threshold = np.percentile(cls_center_dists, 95)
            cls_cutoff = np.searchsorted(cls_center_dists, cls_dist_threshold)
            cls_center = np.median(self._reps[cls_inds[cls_center_dists_sorted_inds]][:cls_cutoff], axis=0)
            cls_threshold = self._calculate_threshold(cls_center, cls_num)
            centers.append(cls_center)
            thresholds.append(cls_threshold)
        self._cluster_centers = np.array(centers, dtype=np.float32)
        self._cluster_thresholds = np.array(thresholds, dtype=np.float32)

    def _calculate_threshold(self, center, label):
        dists = self.get_distances(center)
        dists_sorted_inds = np.argsort(dists)
        labels_bin = np.zeros(self._labels_num.shape, dtype=np.uint8)
        labels_bin[self._labels_num[dists_sorted_inds] == label] = 1
        thresholds = np.linspace(0.0, dists.max(), num=100, endpoint=False, dtype=np.float32)
        optimal_fscore = -1
        optimal_threshold = None
        for threshold in thresholds:
            cutoff = np.searchsorted(dists[dists_sorted_inds], threshold)
            labels_pr = np.zeros(labels_bin.shape, dtype=np.uint8)
            labels_pr[:int(cutoff)] = 1
            fscore = fbeta_score(labels_bin, labels_pr, self._beta)
            if fscore > optimal_fscore:
                optimal_fscore = fscore
                optimal_threshold = threshold
        return optimal_threshold

    def _predict(self, rep, **kwargs):
        confs = self._predict_proba(rep)
        return np.argmax(confs)

    def _predict_proba(self, rep, **kwargs):
        confs1 = self._get_knn_confidences(rep)
        confs2 = self._get_cluster_confidences(rep)
        confs = (self._alpha * confs1) + ((1 - self._alpha) * confs2)
        return confs

    def _get_knn_confidences(self, rep):
        dists = self.get_distances(rep)
        sorted_inds = np.argsort(dists)
        top_label_nums = self._labels_num[sorted_inds][:self._k]
        uniques, counts = np.unique(top_label_nums, return_counts=True)
        confs = np.zeros((len(self.classes)), dtype=np.float32)
        for u, c in zip(uniques, counts):
            try:
                confs[u] = c / float(sum(counts))
            except ZeroDivisionError:
                pass
        return confs

    def get_distances_to_centers(self, rep):
        rep = rep.copy()
        rep = rep.reshape(1, -1)
        distance_metric = self.distance_metric
        if distance_metric.lower().startswith('rectified'):
            distance_metric = distance_metric.split('_')[-1]
            rep[rep < 0] = np.float32(0)
        return cdist(self._cluster_centers, rep, metric=distance_metric).reshape(-1)

    def _get_cluster_confidences(self, rep):
        dists = self.get_distances_to_centers(rep)
        confs = np.clip(dists, self._cluster_thresholds, dists.max())
        confs -= self._cluster_thresholds
        confs /= (dists.max() - self._cluster_thresholds)
        confs = 1 - confs
        confs **= self._gamma
        return confs

    def __repr__(self):
        return '{}(alpha={!r}, fbeta={!r}, k={!r}, gamma={!r}, distance_metric={!r}, config={!r})'.format(type(self).__name__, self._alpha, self._beta, self._k, self._gamma, self._distance_metric, self._config)
