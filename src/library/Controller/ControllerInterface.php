<?php
namespace TKOlomouc\Controller;

interface ControllerInterface
{
    function view($id = null);
    function sidebar();
}
?>