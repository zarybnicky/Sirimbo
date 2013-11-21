<?php
namespace TKOlomouc\Controller;

interface ControllerInterface
{
    public function view($id = null);

    public function sidebar();
}
