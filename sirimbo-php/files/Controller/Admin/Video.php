<?php
namespace Olymp\Controller\Admin;

class Video
{
    public static function orphan()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/Video.twig', [
            'action' => 'orphan',
            'navigation' => '',
            'data' => array_for(\DBVideo::getOrphan(), fn($item) => [
                'type' => 'video',
                'id' => $item['v_id'],
                'title' => $item['v_title'],
                'uri' => explode('?', $item['v_uri'])[0],
                'created' => $item['v_created_at'],
            ]),
        ]);
    }

    public static function playlistList()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/Video.twig', [
            'action' => 'playlist',
            'navigation' => '',
            'data' => array_for(\DBVideoList::getAll(), fn($item) => [
                'type' => 'playlist',
                'id' => $item['vl_id'],
                'title' => $item['vl_title'],
                'uri' => $item['vl_url'],
                'created' => $item['vl_created_at'],
            ]),
        ]);
    }

    public static function playlist($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $list = \DBVideoList::getSingle($id);
        \Render::twig('Admin/Video.twig', [
            'playlist' => $list['vl_title'],
            'action' => 'playlist',
            'navigation' => '',
            'data' => array_for(\DBVideo::getByPlaylist($id), fn($item) => [
                'type' => 'video',
                'id' => $item['v_id'],
                'title' => $item['v_title'],
                'uri' => explode('?', $item['v_uri'])[0],
                'created' => $item['v_created_at'],
            ]),
        ]);
    }

    public static function title()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/VideoTitle.twig', [
            'videos' => \DBVideo::getAll(),
            'video1' => \DBParameters::get('title_video1'),
            'video2' => \DBParameters::get('title_video2'),
            'video3' => \DBParameters::get('title_video3'),
            'video4' => \DBParameters::get('title_video4'),
        ]);
    }

    public static function titlePost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \DBParameters::set('title_video1', $_POST['video1']);
        \DBParameters::set('title_video2', $_POST['video2']);
        \DBParameters::set('title_video3', $_POST['video3']);
        \DBParameters::set('title_video4', $_POST['video4']);
        \Redirect::to('/admin/video/title');
    }
}
