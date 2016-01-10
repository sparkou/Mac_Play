/**
 * Created by spark.ou on 12/29/2015.
 */
var reportApp = angular.module('reportApp')

reportApp.controller('ReportCtrl', function($scope, $uibModal, $http, $templateCache) {
    $scope.users = {
        names: ['Spark', 'Feng', 'Marlon', 'Felix', 'Waikei', 'Kane', 'Hyman', 'Sky', 'Melody', 'Alex', 'Weber']
    }

    $http.get('/report').success(function(data){
        console.log(data);
        $scope.data = data;
        $scope.kitties = data;
    }).error(function(data) {
        console.log('data cannot')
    })

    $scope.today = new Date().toISOString().slice(0, 10);
    $scope.add = function(name) {
        var modalInstance = $uibModal.open({
            animation: false,
            templateUrl: '/assets/partials/newReport.html',
            controller: 'NewReportCtrl',
            size: 'md',
            resolve: {
                name: function() {
                    return name;
                }
            }
        });
        modalInstance.result.then(function (data) {
            $http.get('/report').success(function(data){
                console.log(data);
                $scope.data = data;
                $scope.kitties = data;
            }).error(function(data) {
                console.log('data cannot')
            })
        }, function () {
            //$log.info('Modal dismissed at: ' + new Date());
        });
    }
    $scope.showReport = function(kitty) {
        var modalInstance = $uibModal.open({
            animation: false,
            templateUrl: '/assets/partials/showReport1.html',
            controller: 'ShowReportCtrl',
            windowClass: 'app-modal-window',
            resolve: {
                kitty: function() {
                    return kitty;
                }
            }
        });
        modalInstance.result.then(function (data) {
            $http.get('/report').success(function(data){
                console.log(data);
                $scope.data = data;
                $scope.kitties = data;
            }).error(function(data) {
                console.log('data cannot')
            })
        }, function () {
            //$log.info('Modal dismissed at: ' + new Date());
        });
    }
    
    $scope.showAll = function() {
        var template = $templateCache.get('/assets/partials/showReport1.html');
        console.log(template)

        //var kitties = $scope.kitties;
        //var modalInstance = $uibModal.open({
        //    animation: false,
        //    templateUrl: '/assets/partials/showReport1.html',
        //    controller: 'ShowAllReportCtrl',
        //    windowClass: 'app-modal-window',
        //    resolve: {
        //        kitties: function() {
        //            return kitties;
        //        }
        //    }
        //});
        //modalInstance.result.then(function (data) {
        //    $http.get('/report').success(function(data){
        //        console.log(data);
        //        $scope.data = data;
        //        $scope.kitties = data;
        //    }).error(function(data) {
        //        console.log('data cannot')
        //    })
        //}, function () {
        //    //$log.info('Modal dismissed at: ' + new Date());
        //});
    }

    $scope.editReport =  function(kitty) {
        var modalInstance = $uibModal.open({
            animation: false,
            templateUrl: '/assets/partials/newReport.html',
            controller: 'EditReportCtrl',
            size: 'md',
            resolve: {
                kitty: function() {
                    return kitty;
                }
            }
        });
        modalInstance.result.then(function (data) {
            $http.get('/report').success(function(data){
                console.log(data);
                $scope.data = data;
                $scope.kitties = data;
            }).error(function(data) {
                console.log('data cannot')
            })
        }, function () {
            //$log.info('Modal dismissed at: ' + new Date());
        });

    }
    
    $scope.removeReport = function(kitty) {

        var modalInstance = $uibModal.open({
            animation: false,
            templateUrl: '/assets/partials/confirm.html',
            controller: 'DeleteReportCtrl',
            size: 'sm',
            resolve: {
                kitty: function() {
                    return kitty;
                }
            }
        });
        modalInstance.result.then(function (data) {
            kitty = data;
            $http.delete('/report/'+kitty.date+'/'+kitty.name).success(function(data) {
                alert('Successfully delete Report - '+kitty.date+'-'+kitty.name)
                $http.get('/report').success(function(data){
                    console.log(data);
                    $scope.data = data;
                    $scope.kitties = data;
                }).error(function(data) {
                    console.log('data cannot')
                })
            })
        }, function () {
            //$log.info('Modal dismissed at: ' + new Date());
        });

    }

    $scope.sendEmail = function() {

    }
    getEmail = function() {
        var email = {
            address: "463997636@qq.com",
            subject: "Daily Report",
            //body: "Hey there James, How are things in literature land? Regards, Peter PS I can't figure out how to do carriage returns!"
        };
        email.body = "<div><p>email.subject</p></div>";
        return email;
    };

    $scope.email = getEmail();

 });

reportApp.controller('ShowAllReportCtrl', function($scope, $uibModalInstance, kitties) {
    $scope.kitties = kitties;
    $scope.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
});

reportApp.controller('DeleteReportCtrl', function($scope, $uibModalInstance, kitty) {
    $scope.kitty = kitty;

    $scope.ok = function() {
        $uibModalInstance.close(kitty);
    }
    $scope.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
});

reportApp.controller('EditReportCtrl', function($scope, $uibModalInstance, $http, kitty) {
    $scope.title = "Edit"
    $scope.name = kitty.name;
    $scope.today = kitty.date;
    $scope.isShown = false;
    $scope.buttonShown = true;
    $scope.tasks = kitty.tasks;

    $scope.showTask = function() {
        $scope.task = '';
        $scope.taskOps = {
            types: ['Feature', 'Bug'],
            priorities: ['Critical', 'Major', 'Medium', 'Low'],
            jiraStatuses: ['Open', 'In Progress', 'Resolved', 'Closed'],
            statuses: ['10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', 'Done']
        }
        $scope.isShown = true;
        $scope.buttonShown = false;
    }

    $scope.saveTask = function() {
        $scope.tasks.forEach(function(task) {
            if(task.caseNo == $scope.task.caseNo){
                console.log(task.caseNo);
                $scope.tasks.splice(task);
            }
            console.log($scope.tasks);
        })
        $scope.tasks.push($scope.task);
        $scope.isShown = false;
        $scope.buttonShown = true;
    }

    $scope.showCase = function(task) {
        $scope.isShown = true;
        console.log(task);
        $scope.task = '';
        $scope.taskOps = {
            types: ['Feature', 'Bug'],
            priorities: ['Critical', 'Major', 'Medium', 'Low'],
            jiraStatuses: ['Open', 'In Progress', 'Resolved', 'Closed'],
            statuses: ['10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', 'Done']
        }
        $scope.task = task;
        $scope.buttonShown = false;
    }
    $scope.addReport = function() {
        // write to json

        var team = "THETA";
        var email = "";
        var emailBox = ["spark.ou@misionsky.com", "feng.xuan@missionsky.com", "marlon.xiang@missionsky.com", "felix.huang@missionsky.com", "waikei.tan@missionsky.com",
            "kane.tang@missionsky.com", "hyman.zhang@missionsky.com", "sky.zhang@missionsky.com", "melody.cai@missionsky.com", "alex.li@missionsky.com", "weber.yan@missionsky.com"]
        emailBox.forEach(function(mail){
            if(angular.lowercase(mail).indexOf(angular.lowercase($scope.name)) > -1) {
                email = mail;
            }
        })
        console.log($scope.tasks);
        var kitty = {team: team, name: $scope.name, email: email, date: $scope.today, tasks: $scope.tasks};
        console.log(kitty);
        $http.put('/report/'+kitty.date+'/'+kitty.name, kitty).success(function(data) {

        })

        $uibModalInstance.close(kitty);
    }
    $scope.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
});


reportApp.controller('ShowReportCtrl', function($scope, $uibModalInstance, kitt) {
   $scope.kitty = kitty;
   $scope.cancel = function () {
       $uibModalInstance.dismiss('cancel');
   };
});

reportApp.controller('NewReportCtrl', function($scope, $uibModalInstance, $http, name) {
    $scope.title = "New";
    $scope.name = name;
    $scope.today = new Date().toISOString().slice(0, 10);
    $scope.tasks = [];

    $scope.isShown = false;
    $scope.buttonShown = true;
    $scope.showTask = function() {
        $scope.task = '';
        $scope.taskOps = {
            types: ['Feature', 'Bug'],
            priorities: ['Critical', 'Major', 'Medium', 'Low'],
            jiraStatuses: ['Open', 'In Progress', 'Resolved', 'Closed'],
            statuses: ['10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', 'Done']
        }
        $scope.isShown = true;
        $scope.buttonShown = false;
    }

    $scope.saveTask = function() {
        console.log($scope.task);
        $scope.tasks.push($scope.task);
        $scope.isShown = false;
        $scope.buttonShown = true;
    }

    $scope.showCase = function(task) {
        $scope.isShown = true;
        $scope.task = task;
        $scope.buttonShown = false;
    }
    $scope.addReport = function() {
        // write to json

        var team = "THETA";
        var email = "";
        var emailBox = ["spark.ou@misionsky.com", "feng.xuan@missionsky.com", "marlon.xiang@missionsky.com", "felix.huang@missionsky.com", "waikei.tan@missionsky.com",
            "kane.tang@missionsky.com", "hyman.zhang@missionsky.com", "sky.zhang@missionsky.com", "melody.cai@missionsky.com", "alex.li@missionsky.com", "weber.yan@missionsky.com"]
        emailBox.forEach(function(mail){
            if(angular.lowercase(mail).indexOf(angular.lowercase($scope.name)) > -1) {
                email = mail;
            }
        })

        var kitty = {team: team, name: $scope.name, email: email, date: $scope.today, tasks: $scope.tasks};
        console.log(kitty);
        $http.post('/report', kitty).success(function(data) {

        })

        $uibModalInstance.close(kitty);
    }
    $scope.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
});