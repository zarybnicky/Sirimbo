<?php
trait HelperTrait
{
    public function checkbox(...$args)
    {
        return new CheckboxHelper(...$args);
    }

    public function date(...$args)
    {
        return new DateHelper(...$args);
    }

    public function radio(...$args)
    {
        return new RadioHelper(...$args);
    }

    public function redirect(...$args)
    {
        return new RedirectHelper(...$args);
    }

    public function select(...$args)
    {
        return new SelectHelper(...$args);
    }

    public function submit(...$args)
    {
        return new SubmitHelper(...$args);
    }

    public function text(...$args)
    {
        return new TextHelper(...$args);
    }
}
