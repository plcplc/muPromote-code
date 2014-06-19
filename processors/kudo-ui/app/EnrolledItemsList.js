// A controller for the list of currently enrolled items.
MuPromoteNode.controller("EnrolledItemsListCtrl",
  ['$scope', 'EnrolledItemsService',
  function($scope, enrolledService) {

    // An action to refresh the gui list of enrolled items.
    $scope.refresh = function() {
      $scope.witems = enrolledService.query();
    };

    $scope.refresh();

}]);
