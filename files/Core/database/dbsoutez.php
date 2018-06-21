<?php
class DBSoutez extends Database
{
    public static function addRow($idt, $jmeno, $prijmeni, $p1, $p2, $p3, $p4, $p5)
    {
        self::query(
            "INSERT INTO soutez
            (s_idt,s_jmeno,s_prijmeni,s_1porotce,s_2porotce,s_3porotce,s_4porotce,s_5porotce)
            VALUES ('?','?','?','?','?','?','?','?')",
            $idt, $jmeno, $prijmeni, $p1, $p2, $p3, $p4, $p5
        );
    }

    public static function getByIDT($idt)
    {
        $res = self::query("SELECT * FROM soutez WHERE s_idt='?'", $idt);
        return self::getSingleRow($res);
    }
}
