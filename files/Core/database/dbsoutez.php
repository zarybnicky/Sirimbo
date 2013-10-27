<?php
class DBSoutez extends Database
{
    public static function addRow($idt, $jmeno, $prijmeni, $p1, $p2, $p3, $p4, $p5) {
        list($idt, $jmeno, $prijmeni, $p1, $p2, $p3, $p4, $p5)
            = DBSoutez::escapeArray(array($idt, $jmeno, $prijmeni, $p1, $p2, $p3, $p4, $p5));
        DBSoutez::query("
            INSERT INTO soutez (s_idt,s_jmeno,s_prijmeni,s_1porotce,s_2porotce,s_3porotce,s_4porotce,s_5porotce)
            VALUES ('$idt','$jmeno','$prijmeni','$p1','$p2','$p3','$p4','$p5')
        ");
    }
    public static function getByIDT($idt) {
        list($idt) = DBSoutez::escapeArray(array($idt));

        $res = DBSkupiny::query("SELECT * FROM soutez WHERE s_idt='$idt'");
        return DBSkupiny::getSingleRow($res);
    }
}
?>