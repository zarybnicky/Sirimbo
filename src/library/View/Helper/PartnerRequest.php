<?php
namespace TKOlomouc\View\Helper;

class PartnerRequest
{
    private $_id;

    public function partnerRequest($id = null) {
        if ($id !== null)
            $this->_id = $id;
        else
            $this->_id = User::getUserID();
        return $this;
    }
    public function id($id = null) {
        if ($id === null)
            return $this->_id;
        else
            $this->_id = $id;
        return $this;
    }

    public function getRequestsByMe() {
        $data = DBPary::getPartnerRequestsByMe($this->_id);
        if (empty($data))
            return '';
        $out = '';
        foreach ($data as $item) {
            ob_start();
            ?>
<form action="/member/profil/par/zadost" method="POST">
<div style="width:100%;">
    <span style="float:left;">
        Žádáte uživatele <?php echo $item['u_jmeno'], ' ',$item['u_prijmeni'];?> o partnerství.
    </span>
    <span style="text-align:right;float:right;margin-right:15px;">
        <input type="hidden" name="id" value="<?php echo $item['pn_id'];?>" />
        <button type="submit" name="action" value="cancel">Zrušit</button>
    </span>
</div>
</form>
            <?php
            $out .= notice(ob_get_contents(), true);
            ob_end_clean();
        }
        return $out;
    }
    public function getRequestsForMe() {
        $data = DBPary::getPartnerRequestsForMe($this->_id);
        if (empty($data))
            return '';
        $out = '';
        foreach ($data as $item) {
            ob_start();
            ?>
<form action="/member/profil/par/zadost" method="POST">
<div style="width:100%;">
    <span style="float:left;">
        Uživatel <?php echo $item['u_jmeno'], ' ', $item['u_prijmeni'];?> Vás žádá o partnerství.
    </span>
    <span style="text-align:right;float:right;margin-right:15px;">
        <input type="hidden" name="id" value="<?php echo $item['pn_id'];?>" />
        <button type="submit" name="action" value="accept">Přijmout</button>
        <button type="submit" name="action" value="refuse">Odmítnout</button>
    </span>
</div>
</form>
            <?php
            $out .= notice(ob_get_contents(), true);
            ob_end_clean();
        }
        return $out;
    }
    public function getAll() {
        return ($this->getRequestsByMe() . $this->getRequestsForMe());
    }
}
?>