
// Expose the list of enrolled items as a REST service with ng-resource.
MuPromoteNode.factory("EnrolledItemsService", ['$resource', function($resource) {
  return $resource('/api/enrolledItems', null,
    {
      'save': {method: 'POST', isArray: true},
      'query': {method: 'GET', isArray: true},

    });
}]);

// Expose a single enrolled item as a REST service with ng-resource.
MuPromoteNode.factory("EnrolledItemService", ['$resource', function($resource) {
  return $resource('/test/enrolledItems/:itemId');
}]);
