<?php
class DBPlatbyRaw extends Database
{
    public static function insert($raw, $hash, $sorted, $discarded, $updateValues)
    {
        self::query(
            "INSERT INTO platby_raw
                (pr_raw,pr_hash,pr_sorted,pr_discarded)
            VALUES ('?','?','?','?')
            ON CONFLICT (pr_hash) DO UPDATE SET pr_id=CURRVAL(pg_get_serial_sequence(platby_raw, pr_id))" .
            ($updateValues ? ",pr_sorted=EXCLUDED.pr_sorted,pr_discarded=EXCLUDED.pr_discarded" : ''),
            $raw,
            $hash,
            $sorted,
            $discarded,
        );
        return self::getInsertId();
    }

    public static function update($id, $raw, $hash, $sorted, $discarded)
    {
        self::query(
            "UPDATE platby_raw
            SET pr_raw='?',pr_hash='?',pr_sorted='?',pr_discarded='?'
            WHERE pr_id='$id'",
            $raw,
            $hash,
            $sorted,
            $discarded,
            $id,
        );
    }

    public static function skip($id)
    {
        self::query(
            "REPLACE INTO platby_raw
            (pr_raw,pr_hash,pr_sorted,pr_discarded)
            SELECT pr_raw,pr_hash,pr_sorted,pr_discarded
            FROM platby_raw
            WHERE pr_id='?'",
            $id
        );
    }

    public static function delete($id)
    {
        self::query("DELETE FROM platby_raw WHERE pr_id='?'", $id);
    }

    public static function getUnsorted()
    {
        $res = self::query("SELECT * FROM platby_raw WHERE pr_sorted='0' AND pr_discarded='0' ORDER BY pr_id");
        return self::getArray($res);
    }

    public static function getDiscarded()
    {
        $res = self::query("SELECT * FROM platby_raw WHERE pr_discarded='1'");
        return self::getArray($res);
    }

    public static function getSingle($id)
    {
        $res = self::query("SELECT * FROM platby_raw WHERE pr_id='?'", $id);
        return self::getSingleRow($res);
    }
}
