<?php
class Buttons
{
    public static function edit($url)
    {
        return "<a href='$url' title='Upravit'>" .
            "<img alt='Upravit' src='/style/icon-pencil.png' />" .
            "</a>";
    }

    public static function duplicate($url)
    {
        // return "<button style='padding:0' name='action' value='{$this->link}'>" .
        //     "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
        //     "</button>";
        return "<a href='$url' title='Duplikovat'>" .
            "<img alt='Duplikovat' src='/style/icon-files-o.png' />" .
            "</a>";
    }

    public static function delete($url)
    {
        return "<a href='$url' title='Odstranit'>" .
            "<img alt='Odstranit' src='/style/icon-trash-o.png' />" .
            "</a>";
    }

    public static function video($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/video/edit/$id"),
            self::delete("/admin/video/remove/$id"),
        ]);
    }

    public static function videoSource($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/video/source/edit/$id"),
            self::delete("/admin/video/source/remove/$id"),
        ]);
    }

    public static function document($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/dokumenty/edit/$id"),
            self::delete("/admin/dokumenty/remove/$id"),
        ]);
    }

    public static function platbyCategory($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/platby/structure/category/edit/$id"),
            self::delete("/admin/platby/structure/category/remove/$id"),
        ]);
    }

    public static function platbyGroup($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/platby/structure/group/edit/$id"),
            self::delete("/admin/platby/structure/group/remove/$id"),
        ]);
    }

    public static function platbyItem($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/platby/items/edit/$id"),
            self::delete("/admin/platby/items/remove/$id"),
        ]);
    }

    public static function permission($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/permissions/edit/$id"),
            self::delete("/admin/permissions/remove/$id"),
        ]);
    }

    public static function skupiny($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/skupiny/edit/$id"),
            self::delete("/admin/skupiny/remove/$id"),
        ]);
    }

    public static function pary($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/pary/edit/$id"),
            self::delete("/admin/pary/remove/$id"),
        ]);
    }

    public static function user($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/users/edit/$id"),
            self::delete("/admin/users/remove/$id"),
        ]);
    }

    public static function nastenka($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/nastenka/edit/$id"),
            self::delete("/admin/nastenka/remove/$id"),
        ]);
    }

    public static function galleryDir($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/galerie/directory/edit/$id"),
            self::duplicate("/admin/galerie/directory/$id"),
            self::delete("/admin/galerie/directory/remove/$id"),
        ]);
    }

    public static function galleryFoto($id)
    {
        return implode('&nbsp;', [
            self::edit("/admin/galerie/file/edit/$id"),
            self::delete("/admin/galerie/file/remove/$id"),
        ]);
    }
}
