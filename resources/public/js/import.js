(function () {
  angular.module('clj-money-import', [])
    .factory('apiClient', ['$http', function($http) {
      return {
        createImport: function(attributes, antiForgeryToken) {
          var data = new FormData();
          for(var key in attributes)
            data.append(key, attributes[key]);
          return $http.post("/api/imports", data, {
            headers: {
              "Content-Type": undefined,
              "X-CSRF-Token": antiForgeryToken
            }
          });
        },
        getImport: function(importId) {
          return $http.get("/api/imports/" + importId);
        }
      };
    }])
    .directive('fileModel', ['$parse', function($parse) {
      return {
        restrict: 'A',
        link: function(scope, element, attrs) {
          var model = $parse(attrs.fileModel);
          var modelSetter = model.assign;

          element.bind('change', function() {
            scope.$apply(function() {
              modelSetter(scope, element[0].files[0]);
            });
          });
        }
      };
    }])
    .controller('ImportController', ['$scope', 'apiClient', function($scope, apiClient) {
      $scope.activeImport = null;
      $scope.alerts = [];
      $scope.statusMessage = null;

      var importIsComplete = function(imp) {
        var progress = imp.progress;
        if (_.isEmpty(progress))
          return false;
        return _.every(progress, function(prop) {
          return prop.total == prop.imported;
        });
      };

      var updateProgressBars = function(progress) {
        _.chain(progress)
          .keys()
          .each(function(key) {
            var progress = $scope.activeImport.progress[key];
            var imported = 0;
            if (!_.isUndefined(progress.imported))
              imported = progress.imported;
            $("#progress-" + key)
              .progressbar({
                max: progress.total
              })
              .progressbar("value", imported);
        }).value();
      };

      var trackImportProgress = function() {
        var trackingId = window.setInterval(function() {
          try {
            apiClient.getImport($scope.activeImport.id).then(function(response) {
              $scope.activeImport = response.data;
              window.setTimeout(function() {
                updateProgressBars($scope.activeImport.progress);
              }, 250);
              if (importIsComplete($scope.activeImport)) {
                $scope.statusMessage = null;
                $scope.alerts.push({
                  message: "Import complete.",
                  level: 'success'
                });
                window.clearInterval(trackingId);
              }
            }, function(error) {
              console.log("Unable to get the updated import");
              console.log(error);
              $scope.statusMessage = null;
              $scope.alerts.push({
                message: "Import failed: " + error.statusText,
                level: 'danger'
              });
              window.clearInterval(trackingId);
            });
          } catch (e) {
            window.clearInterval(trackingId);
            $scope.statusMessage = null;
            $scope.alerts.push({
              message: "Import failed: " + e,
              level: 'danger'
            });
          }
        }, 1000);
      };

      $scope.startImport = function() {
        $scope.statusMessage = "Uploading the files...";
        $scope.alerts.length = 0;
        apiClient.createImport({
          "entity-name": $scope.entityName,
          "source-file-0": $scope.sourceFile0,
          "source-file-1": $scope.sourceFile1,
          "source-file-2": $scope.sourceFile2,
          "source-file-3": $scope.sourceFile3,
          "source-file-4": $scope.sourceFile4,
          "source-file-5": $scope.sourceFile5,
          "source-file-6": $scope.sourceFile6,
          "source-file-7": $scope.sourceFile7,
          "source-file-8": $scope.sourceFile8,
          "source-file-9": $scope.sourceFile9,
        }, $scope.antiForgeryToken).then(function(response) {
          $scope.statusMessage = "Processing the files...";
          $scope.activeImport = response.data.import;
          trackImportProgress();
        },
        function(error) {
          console.log("Unable to create the import");
          console.log(error);
        });
      };
    }])
  .run(['$http', function($http) {
    $http.defaults.headers.common['Content-Type'] = "application/json";
    $http.defaults.headers.common['Accept'] = 'application/json';
  }]);
})();
