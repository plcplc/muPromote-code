
// Define a controller for enrolling promotable items.
MuPromoteNode.controller("PromotableItemEnrollerCtrl",
  ['$scope', 'EnrolledItemsService',
  function($scope, enrolledService) {

    $scope.itemObject = null;
    $scope.itemText = null;
    $scope.itemTextStatusMessage = "";
    $scope.enrollingDisabled = true;
    $scope.weight = 1.0;

    // Watcher to parse 'itemText' to 'itemObject'.
    $scope.$watch('itemText', function(newVal) {
      try {
        $scope.itemObject = angular.fromJson(newVal);
        if (typeof $scope.itemObject == 'object') {
          $scope.itemTextStatusMessage = "";
          $scope.enrollingDisabled = false;
        } else {
          $scope.itemTextStatusMessage = "Only JSON objects are supported. " + typeof $scope.itemObject + " given.";
          $scope.itemObject = null;
          $scope.enrollingDisabled = true;
        }
      } catch (err) {
        $scope.itemObject = null;
        $scope.itemTextStatusMessage = err.name + ": " + err.message;
        $scope.enrollingDisabled = true;
      }
    });

    // Action to enroll items.
    $scope.enrollItem = function() {
      $scope.resp = enrolledService.save([[$scope.weight, $scope.itemObject]]);
    };

}]);
