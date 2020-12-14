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

    public function redirect(...$args)
    {
        return new RedirectHelper(...$args);
    }
}
