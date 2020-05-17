<?php
class DBPlatbyRaw extends Database
{
    public static function insert($raw, $hash, $sorted, $discarded, $updateValues)
    {
        list($raw, $hash, $sorted, $discarded) =
            self::escape($raw, $hash, $sorted, $discarded);

        self::query(
            "INSERT INTO platby_raw
                (pr_raw,pr_hash,pr_sorted,pr_discarded)
            VALUES
                ('$raw','$hash','$sorted','$discarded')
            ON DUPLICATE KEY UPDATE
                pr_id=LAST_INSERT_ID(pr_id)" .
            ($updateValues ? ",pr_sorted=VALUES(pr_sorted),
                pr_discarded=VALUES(pr_discarded)" : '')
        );
        return self::getInsertId();
    }

    public static function update($id, $raw, $hash, $sorted, $discarded)
    {
        list($id, $raw, $hash, $sorted, $discarded) =
            self::escape($id, $raw, $hash, $sorted, $discarded);

        self::query(
            "UPDATE platby_raw
            SET pr_raw='$raw',pr_hash='$hash',pr_sorted='$sorted',pr_discarded='$discarded'
            WHERE pr_id='$id'"
        );
    }

    public static function skip($id)
    {
        list($id) = self::escape($id);
        self::query(
            "REPLACE INTO platby_raw
            (pr_raw,pr_hash,pr_sorted,pr_discarded)
            SELECT pr_raw,pr_hash,pr_sorted,pr_discarded
            FROM platby_raw
            WHERE pr_id='$id'"
        );
    }

    public static function delete($id)
    {
        list($id) = self::escape($id);
        self::query("DELETE FROM platby_raw WHERE pr_id='$id'");
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
        list($id) = self::escape($id);

        $res = self::query("SELECT * FROM platby_raw WHERE pr_id='$id'");
        return self::getSingleRow($res);
    }
}
