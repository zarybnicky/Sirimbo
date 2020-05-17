<?php
trait HelperTrait
{
    public function bsRadio()
    {
        return call_user_func_array([new BsRadioHelper(), 'bsRadio'], func_get_args());
    }

    public function checkbox()
    {
        return call_user_func_array([new CheckboxHelper(), 'checkbox'], func_get_args());
    }

    public function colorbox()
    {
        return call_user_func_array([new ColorboxHelper(), 'colorbox'], func_get_args());
    }

    public function date()
    {
        return call_user_func_array([new DateHelper(), 'date'], func_get_args());
    }

    public function duplicateLink()
    {
        return call_user_func_array([new DuplicateLinkHelper(), 'duplicateLink'], func_get_args());
    }

    public function editLink()
    {
        return call_user_func_array([new EditLinkHelper(), 'editLink'], func_get_args());
    }

    public function hidden()
    {
        return call_user_func_array([new HiddenHelper(), 'hidden'], func_get_args());
    }

    public function login()
    {
        return call_user_func_array([new LoginHelper(), 'login'], func_get_args());
    }

    public function navbarItem()
    {
        return call_user_func_array([new NavbarItemHelper(), 'navbarItem'], func_get_args());
    }

    public function notice()
    {
        return call_user_func_array([new NoticeHelper(), 'notice'], func_get_args());
    }

    public function partnerRequest()
    {
        return call_user_func_array([new PartnerRequestHelper(), 'partnerRequest'], func_get_args());
    }

    public function person()
    {
        return call_user_func_array([new PersonHelper(), 'person'], func_get_args());
    }

    public function radio()
    {
        return call_user_func_array([new RadioHelper(), 'radio'], func_get_args());
    }

    public function redirect()
    {
        return call_user_func_array([new RedirectHelper(), 'redirect'], func_get_args());
    }

    public function removeLink()
    {
        return call_user_func_array([new RemoveLinkHelper(), 'removeLink'], func_get_args());
    }

    public function select()
    {
        return call_user_func_array([new SelectHelper(), 'select'], func_get_args());
    }

    public function submit()
    {
        return call_user_func_array([new SubmitHelper(), 'submit'], func_get_args());
    }

    public function table()
    {
        return call_user_func_array([new TableHelper(), 'table'], func_get_args());
    }

    public function text()
    {
        return call_user_func_array([new TextHelper(), 'text'], func_get_args());
    }

    public function userSelect()
    {
        return call_user_func_array([new UserSelectHelper(), 'userSelect'], func_get_args());
    }
}
