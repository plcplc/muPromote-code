
// Test the PromotableItemEnroller controller.
describe("The PromotableItemEnroller controller", function() {

  // Save the scope for local reference.
  var $scope;

  // Wire up the module and controller.
  beforeEach(module('MuPromoteNode'));
  beforeEach(inject(function($rootScope, $controller) {
    $scope = $rootScope.$new();
    $controller('PromotableItemEnrollerCtrl', {$scope: $scope});
  }));

  it("rejects enrolling on non-parsing items", function() {

    $scope.itemText = "not json";
    $scope.$digest();

    expect($scope.itemObject).toBe(null);
    expect($scope.enrollingDisabled).toBe(true);

  });

  it("accepts enrolling on parsing items", function() {

    var item = {this: "should", "be": ["proper","json"]};
    $scope.itemText = angular.toJson(item);
    $scope.$digest();

    expect($scope.itemObject).toEqual(item);
    expect($scope.enrollingDisabled).toBe(false);

  });

});
