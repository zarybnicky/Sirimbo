<?php
namespace Olymp\Controller\Member;

class Schedule
{
    public static function get()
    {
        \Permissions::checkError('rozpis', P_VIEW);
        \Permissions::checkError('nabidka', P_VIEW);
        \Render::twig('Member/Schedule.twig');
    }
}
