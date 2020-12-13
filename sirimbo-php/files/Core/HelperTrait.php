<?php
trait HelperTrait
{
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

    public function hidden()
    {
        return call_user_func_array([new HiddenHelper(), 'hidden'], func_get_args());
    }

    public function login()
    {
        return call_user_func_array([new LoginHelper(), 'login'], func_get_args());
    }

    public function notice()
    {
        return call_user_func_array([new NoticeHelper(), 'notice'], func_get_args());
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

    public function select()
    {
        return call_user_func_array([new SelectHelper(), 'select'], func_get_args());
    }

    public function submit()
    {
        return call_user_func_array([new SubmitHelper(), 'submit'], func_get_args());
    }

    public function text()
    {
        return call_user_func_array([new TextHelper(), 'text'], func_get_args());
    }
}
