<?php
class Utils
{
    public static function submit(string $text, ?string $name = null, ?string $value = null, string $cls = 'btn btn-primary'): string
    {
        return "<button name=\"$name\" value=\"$value\" class=\"$cls\">$text</button>";
    }

    public static function person(array $user): string
    {
        return "<a href=\"/member/clenove/{$user['u_id']}\">"
            . '<img src="/style/person-small.png" style="margin-bottom:-2px" src="'
            . "{$user['u_jmeno']} {$user['u_prijmeni']}\">&nbsp;"
            . "{$user['u_prijmeni']}, {$user['u_jmeno']}</a>";
    }

    public static function notice(string $text, string $type = "info"): string
    {
        if (!$text) {
            return '';
        }
        return "<div class=\"alert alert-$type\" role=\"alert\">$text</div>";
    }

    public static function text(string $name, $value = null, array $options = []): string
    {
        if ($value === null) {
            $value = $name;
        }
        return (string) new Tag(
            'input',
            [
                'type' => 'text',
                'name' => $name,
                'value' => $value,
                'class' => $options['cls'] ?? 'form-control',
                'size' => $options['size'] ?? null,
                'placeholder' => $options['placeholder'] ?? null,
                'readonly' => $options['readonly'] ?? false,
                'disabled' => $options['disabled'] ?? false,
            ]
        );
    }

    public static function checkbox(string $name, ?string $value = null, $state = false, ?string $label = null): string
    {
        if ($value === null) {
            $value = $name;
        }
        $checked = $state ? 'checked="checked"' : '';
        return "<div class='form-group form-check'>"
            . "<input type=checkbox class='form-check-input' name='{$name}'"
            . " value='$value' $checked>"
            . ($label ? "<label class='form-check-label'>{$label}</label>" : '')
            . "</div>";
    }

    public static function radio(string $name, ?string $value = null, $state = false, ?string $label = null): string
    {
        if ($value === null) {
            $value = $name;
        }
        $checked = $state ? 'checked="checked"' : '';
        return "<div class='form-group form-check'>"
            . "<input type=radio class='form-check-input' name='{$name}'"
            . " value='$value' $checked>"
            . ($label ? "<label class='form-check-label'>{$label}</label>" : '')
            . "</div>";
    }

    public static function inlineRadio(string $name, ?string $value = null, $state = false, ?string $label = null): string
    {
        if ($value === null) {
            $value = $name;
        }
        $checked = $state ? 'checked="checked"' : '';
        return "<div class='form-check form-check-inline'>"
            . "<input type=radio class='form-check-input' name='$name'"
            . " value='$value' $checked>"
            . ($label ? "<label class='form-check-label'>$label</label>" : '')
            . "</div>";
    }

    public static function userSelect(array $data, string $name = 'user', $set = null, string $index = 'u_id'): string
    {
        $out = "<div class=\"$name\">\n";
        $out .= "<select class=\"form-control select2\" name=\"$name\">\n";
        if (!$set) {
            $out .= "<option value=\"0\" selected=\"selected\">--- žádný ---</option>\n";
        } else {
            $out .= "<option value=\"0\">--- žádný ---</option>\n";
        }
        foreach ($data as $user) {
            $id = $user[$index];
            $out .= "<option value=\"$id";
            $out .= $id == $set ? '" selected="selected">' : '">';
            $out .= $user['u_prijmeni'] . ', ' . $user['u_jmeno'];
            if (isset($user['u_narozeni'])) {
                $out .= ', ' . explode('-', $user['u_narozeni'])[0];
            }
            $out .= "</option>\n";
        }
        $out .= "</select>\n";
        $out .= '</div>';
        return $out;
    }

    public static function selectAssoc(string $name, array $data, string $key, string $value, $set = null): string
    {
        $options = array_map(
            fn($x) => "<option value=\"{$x[$key]}\""
            . ((string) $set === (string) $x[$key] ? " selected" : '')
            . ">{$x[$value]}</option>",
            $data,
        );
        return "<select class=\"form-control select2\" name=\"$name\">" . implode($options) . "</select>";
    }

    public static function select(string $name, array $data, $set = null): string
    {
        $options = array_map(
            fn($k, $v) => "<option value=\"$k\""
            . ((string) $set === (string) $k ? " selected" : '')
            . ">$v</option>",
            array_keys($data),
            array_values($data),
        );
        return "<select class=\"form-control select2\" name=\"$name\">" . implode($options) . "</select>";
    }

    public static function selectLiteral(string $name, array $data, $set = null): string
    {
        $options = array_map(
            fn($x) => "<option value=\"$x\""
            . ((string) $set === (string) $x ? " selected" : '')
            . ">$x</option>",
            $data,
        );
        return "<select name=\"$name\">" . implode($options) . "</select>";
    }

    public static function partnerRequest(): string
    {
        $out = '';
        foreach (\DBPary::getPartnerRequestsByMe(\Session::getUser()->getId()) as $item) {
            ob_start();
            ?>
<form action="/member/profil/par/zadost" method="POST">
<div style="width:100%;">
    <span style="float:left;">
        Žádáte uživatele <?= $item['u_jmeno'], ' ', $item['u_prijmeni'] ?> o partnerství.
    </span>
    <span style="text-align:right;float:right;margin-right:15px;">
        <input type="hidden" name="id" value="<?= $item['pn_id'] ?>" />
        <button type="submit" name="action" value="cancel">Zrušit</button>
    </span>
    <div style="clear:both"></div>
</div>
</form>
            <?php
            $out .= \Utils::notice(ob_get_contents());
            ob_end_clean();
        }
        foreach (\DBPary::getPartnerRequestsForMe(\Session::getUser()->getId()) as $item) {
            ob_start();
            ?>
<form action="/member/profil/par/zadost" method="POST">
<div style="width:100%;">
    <span style="float:left;">
        Uživatel <?= $item['u_jmeno'], ' ', $item['u_prijmeni'] ?> Vás žádá o partnerství.
    </span>
    <span style="text-align:right;float:right;margin-right:15px;">
        <input type='hidden' name='id' value='<?= $item['pn_id'] ?>'>
        <button type="submit" name="action" value="accept">Přijmout</button>
        <button type="submit" name="action" value="refuse">Odmítnout</button>
    </span>
    <div style="clear:both"></div>
</div>
</form>
            <?php
            $out .= \Utils::notice(ob_get_contents());
            ob_end_clean();
        }
        return $out;
    }

    public static function navbarItem($url, $item)
    {
        if (!is_array($item)) {
            return "<div class='$item'></div>";
        }
        if (isset($item[3]) && $item[3] && !\Permissions::check($item[3][0], $item[3][1])) {
            return '';
        }
        $active = $item[1] === ('/' . $url)
            || (strlen($item[1]) > 1 && strpos('/' . $url, $item[1]) === 0);
        $active = $active ? ' active' : '';

        if (!isset($item[2]) || empty($item[2])) {
            return '<li class="nav-item' . $active . '"><a class="nav-link" href="'
                . $item[1] . '">' . $item[0] . '</a></li>';
        }
        $x = '<a class="nav-link dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">' . $item[0] . '</a>';
        $x .= '<div class="dropdown-menu">';
        foreach ($item[2] as $sub) {
            if (isset($sub[3]) && $sub[3] && !\Permissions::check($sub[3][0], $sub[3][1])) {
                continue;
            }
            $x .= '<a class="dropdown-item" href="' . $sub[1] . '">' . $sub[0] . '</a>';
        }
        $x .= '</div>';
        return '<li class="nav-item' . $active . ' dropdown">' . $x . '</li>';
    }

    public static function date($name, $date, $cls = 'form-control'): string
    {
        $dateObj = new \Date($date);
        $val = $dateObj->isValid() ? $dateObj->getHumanDate() : $date;
        return "<input type=\"text\" name=\"$name\" value=\"$val\" class=\"$cls\">";
    }

    public static function dateRange($name, $date, $cls = 'form-control'): string
    {
        return (string) (new \DateHelper($name, $date))->cls($cls)->range();
    }

    public static function table($data, $columns, $style = ''): string
    {
        return (string) new \TableHelper($data, $columns, $style);
    }
}
