<?php
class DBPlatbyRaw extends Database
{
    public static function insert($raw, $hash, $sorted, $discarded)
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
        $row = self::querySingle("SELECT pr_id FROM platby_raw WHERE pr_hash='?'", $hash);
        return $row['pr_id'];
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

    public static function getUnsorted()
    {
        return self::queryArray("SELECT * FROM platby_raw WHERE pr_sorted='0' AND pr_discarded='0' ORDER BY pr_id");
    }

    public static function getDiscarded()
    {
        return self::queryArray("SELECT * FROM platby_raw WHERE pr_discarded='1'");
    }

    public static function getSingle($id)
    {
        return self::querySingle("SELECT * FROM platby_raw WHERE pr_id='?'", $id);
    }
}
