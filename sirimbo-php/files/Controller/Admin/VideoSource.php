<?php
namespace Olymp\Controller\Admin;

class VideoSource
{
    public static function add()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add');
        }
        \DBVideoSource::add($_POST['uri']);
        \Redirect::to('/admin/video/source');
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideoSource::getSingle($id)) {
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/video/source');
        }
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideoSource::getSingle($id)) {
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/video/source');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit', $data);
        }
        \DBVideoSource::edit($id, $_POST['uri'], $_POST['title'], $_POST['desc']);
        \Redirect::to('/admin/video/source');
    }

    protected static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/VideoSourceForm.twig', [
            'action' => $action,
            'id' => $data ? $data['vs_id'] : null,
            'uri' => $_POST['uri'] ?? $data['vs_url'] ?? '',
            'title' => $_POST['title'] ?? $data['vs_title'] ?? '',
            'desc' => $_POST['desc'] ?? $data['vs_description'] ?? '',
        ]);
    }

    protected static function checkData()
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['uri'], 'Zadejte prosím ID kanálu');
        return $form;
    }
}
