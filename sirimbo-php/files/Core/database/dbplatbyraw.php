<?php
class DBPlatbyRaw extends Database
{
    public static function insert($raw, $hash, $sorted, $discarded, $updateValues)
    {
        self::query(
            "INSERT INTO platby_raw (pr_raw,pr_hash,pr_sorted,pr_discarded)
            VALUES ('?','?','?','?')
            ON CONFLICT (pr_hash) DO UPDATE SET pr_discarded=false, pr_sorted=EXCLUDED.pr_sorted
            RETURNING pr_id",
            $raw,
            $hash,
            $sorted,
            $discarded,
        );
        $res = self::query("SELECT pr_id FROM platby_raw WHERE pr_hash='?'", $hash);
        return self::getArray($res)['pr_id'];
    }

    public static function update($id, $raw, $hash, $sorted, $discarded)
    {
        self::query(
            "UPDATE platby_raw
            SET pr_raw='?',pr_hash='?',pr_sorted='?',pr_discarded='?'
            WHERE pr_id='?'",
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
            "WITH deletions AS (DELETE FROM platby_raw WHERE pr_id='?'
               RETURNING pr_raw,pr_hash,pr_sorted,pr_discarded)
            INSERT INTO platby_raw (pr_raw,pr_hash,pr_sorted,pr_discarded)
               SELECT * from deletions",
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
