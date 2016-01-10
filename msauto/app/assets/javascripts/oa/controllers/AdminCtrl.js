/**
 * OACtrl Controller
 */

var oa = angular.module('MSDash');
oa.controller('AdminCtrl', function($scope, $http, $uibModal) {

    $scope.closeAlert = function() {
        $scope.alertShow = false;
    };

    showAlert = function(type, msg) {
        $scope.alertShow = true;
        $scope.alert = {type: type, msg: msg};
    }



    $scope.isManage = false;

    getUsers =  function() {
        $http.get('/users').success(function(data) {
            $scope.users = data;
            $scope.user = data[0];
        })
    }
    getUsers();
    $scope.addUser = function() {
        $scope.isManage = !$scope.isManage;
        $http.get('/userInit').success(function(data) {
            $scope.user = data;
            $scope.userStats = {
                statuses: ['Active', 'Inactive']
            }
        });
    }

    $scope.showUser = function(user) {
        $scope.user = user;
    }

    //$scope.refreshUser = function() {
    //    getUsers().then(function(data) {
    //        // loading...
    //    });
    //}

    $scope.removeUser = function(user) {
        var modalInstance = $uibModal.open({
            animation: false,
            templateUrl: '/assets/templates/oa/confirm.html',
            controller: 'DeleteUserCtrl',
            size: 'sm',
            resolve: {
                user: function() {
                    return user;
                }
            }
        });
        modalInstance.result.then(function (data) {
            showAlert('danger', 'User Successfully Removed!');
            getUsers();
        }, function () {

        });
    }

    //$scope.createUser = function(user) {
    //    $http.get('/user/'+user.eName+'/'+user.cName).success(function(data) {
    //        if(data.length == 0) {
    //            $http.post('/user', user).success(function(data) {
    //                console.log(data);
    //                showAlert('success', 'User Successfully Created!');
    //                getUsers();
    //            })
    //        }else {
    //            showAlert('info', 'User Already Existed!')
    //        }
    //    })
    //    $scope.isManage = !$scope.isManage;
    //}

    $scope.editUser = function(user) {
        $scope.userStats = {
            statuses: ['Active', 'Inactive']
        }
        $scope.isManage = !$scope.isManage;
    }

    $scope.updateUser = function(user) {
        if(angular.isUndefined(user.eName) || user.eName == '' || angular.isUndefined(user.cName) ||  user.cName == '') {
            showAlert('danger', 'User Info is not correct!')
        }else{
            $http.get('/user/'+user.eName+'/'+user.cName).success(function(data) {
                if(data.length == 0) {
                    $http.post('/user', user).success(function(data) {
                        console.log(data);
                        showAlert('success', 'User Successfully Created!');
                        getUsers();
                    })
                }else {
                    $http.put('/user/'+user.eName+'/'+user.cName, user).success(function(data) {
                        showAlert('info', 'User Successfully Updated!')
                        getUsers();
                    })
                }
            })
            $scope.isManage = !$scope.isManage;

        }
    }
    $scope.cancelUser = function() {
        $scope.isManage = !$scope.isManage;
    }
});

oa.controller('DeleteUserCtrl', function($scope, $http, $uibModalInstance, user) {

    $scope.user = user;
    $scope.ok = function(user) {
        $http.delete('/user/'+user.eName+'/'+user.cName).success(function(data) {
            console.log(data);
        })
        $uibModalInstance.close('ok');
    }
    $scope.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
});