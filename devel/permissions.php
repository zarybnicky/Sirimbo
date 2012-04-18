<?php
$oML = new ModuleLoader();
echo $oML->get( 'Poll' );

class Permission
{
    function Permission() { }
    
    function get( $sName, $mNameId, $oUser ) {
        // 1. get all permissions where:
        // name == user && name_id == $oUser->getId();
        // && target == $sName && target_id == $mNameId
        // if found return allow = true, deny = false
        
        // 2. get all permissions where:
        // name == group && target == $sName && target_id == $mNameId
        // if found return $this->belongsToGroup( name_ids, $oUser->getGroupIds() )
        
        // 3. return false as it is default
        return true;
    }
    function belongsToGroup( $aPermissionGroups, $aUserGroups ) {
        // solve hierarcal group theme
    }
}

class ModuleLoader
{
    function get( $sName ) {
        
        $oPermission = new Permission();
        
        if( $oPermission->get( 'module',
                                Request::getModule(), 
                                Login::getUser() ) ) {
            $oMod = new Module( $sName );
            $oMod->action();
            $oMod->view();
            return '<br />Getting module: '.Request::getModule().$oMod->sContent;
        } else {
            return '<br />Permission denied for module: '.Request::getModule();
        }
    }

}

class Module
{
    var $sContent;
    var $sName;
    
    function Module( $sName ) {
        $this->sName = $sName;
        $this->sContent = '';
    }
    
    function view() {
        
        $oPermission = new Permission();
        
        if( $oPermission->get( 'view',
                                Request::getView(), 
                                Login::getUser() ) ) {
            $this->sContent .= '<br />Including view: '.Request::getView();
        } else {
            $this->sContent .= '<br />Permission denied for view: '.Request::getView();
        }
    }
    
    function action() {
        
        $oPermission = new Permission();
        
        if( $oPermission->get( 'action',
                                Request::getAction(), 
                                Login::getUser() ) ) {
            $this->sContent .= '<br />Including action: '.Request::getAction();
        } else {
            $this->sContent .= '<br />Permission denied for action: '.Request::getAction();
        }
    }
}

class Request
{
    function getModule() { return 'Poll'; }
    function getView() { return 'PollShowResults'; }
    function getAction() { return 'PollAdd'; }
    function getItem() { return 5; }
}

class Login
{
    function getUser() { return new User; }
}

class User 
{
    function getId() { return 3; }
    function getGroupIds() { return array( 407, 34 ); }
}
?>
