<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Model\DBPary;
use TKOlomouc\Utility\User;

class PartnerRequest extends Partial
{
    private $userId = null;

    public function __construct($id = null)
    {
        if ($id === null) {
            $id = User::getUserID();
        }

        $this->userId = $id;
        return $this;
    }

    public function setId($id = null)
    {
        $this->userId = $id;

        return $this;
    }

    public function getRequestsByMe()
    {
        if (!($data = DBPary::getPartnerRequestsByMe($this->userId))) {
            return '';
        }
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
            $out .= notice(ob_get_clean(), true);
        }
        return $out;
    }
    public function getRequestsForMe()
    {
        if ($data = DBPary::getPartnerRequestsForMe($this->userId)) {
            return '';
        }
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
            $out .= notice(ob_get_clean(), true);
        }
        return $out;
    }
    public function getAll()
    {
        return ($this->getRequestsByMe() . $this->getRequestsForMe());
    }
}
