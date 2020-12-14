<?php
class PartnerRequestHelper
{
    private $_id;

    public function __construct($id = null)
    {
        $this->_id = $id !== null ? $id : Session::getUserID();
        return $this;
    }

    public function id($id = null)
    {
        if ($id === null) {
            return $this->_id;
        }
        $this->_id = $id;
        return $this;
    }

    public function getRequestsByMe()
    {
        $data = DBPary::getPartnerRequestsByMe($this->_id);
        if (empty($data)) {
            return '';
        }
        $out = '';
        foreach ($data as $item) {
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
            $out .= new NoticeHelper(ob_get_contents());
            ob_end_clean();
        }
        return $out;
    }

    public function getRequestsForMe()
    {
        $data = DBPary::getPartnerRequestsForMe($this->_id);
        if (empty($data)) {
            return '';
        }
        $out = '';
        foreach ($data as $item) {
            ob_start();
            ?>
<form action="/member/profil/par/zadost" method="POST">
<div style="width:100%;">
    <span style="float:left;">
        Uživatel <?= $item['u_jmeno'], ' ', $item['u_prijmeni'] ?> Vás žádá o partnerství.
    </span>
    <span style="text-align:right;float:right;margin-right:15px;">
        <?= new HiddenHelper('id', $item['pn_id']) ?>
        <button type="submit" name="action" value="accept">Přijmout</button>
        <button type="submit" name="action" value="refuse">Odmítnout</button>
    </span>
    <div style="clear:both"></div>
</div>
</form>
            <?php
            $out .= new NoticeHelper(ob_get_contents());
            ob_end_clean();
        }
        return $out;
    }

    public function __toString()
    {
        return $this->getRequestsByMe() . $this->getRequestsForMe();
    }
}
