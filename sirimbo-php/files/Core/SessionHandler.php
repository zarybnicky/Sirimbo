<?php
class DbSessionHandler extends Database implements SessionHandlerInterface
{
    public function open($savePath, $sessionName)
    {
        return true;
    }

    public function read($sessionId)
    {
        list($id) = self::escape($sessionId);
        $res = self::query(
            "SELECT * FROM session WHERE
             ss_id='$id' AND
             (UNIX_TIMESTAMP(ss_updated_at) + ss_lifetime) > UNIX_TIMESTAMP()"
        );
        if (!$res || !($result = self::getSingleRow($res))) {
            return '';
        }
        return serialize(json_decode($result['ss_data'], true));
    }

    public function write($sessionId, $data)
    {
        setcookie(session_name(), $sessionId, time() + 86400, '/', 'tkolymp.cz');

        $data = json_encode(unserialize($data), JSON_FORCE_OBJECT);
        list($id, $data) = self::escape($sessionId, $data);
        return !!self::query(
            "INSERT INTO session
             (ss_id, ss_data, ss_lifetime) VALUES
             ('$id', '$data', 86400)
             ON DUPLICATE KEY UPDATE
             ss_data='$data', ss_updated_at=NOW()"
        );
    }

    public function gc($maxLifetime)
    {
        return !!self::query(
            "DELETE FROM session WHERE
             (UNIX_TIMESTAMP(ss_updated_at) + ss_lifetime) < UNIX_TIMESTAMP()"
        );
    }

    public function destroy($sessionId)
    {
        setcookie(session_name(), '', -1, '/');

        list($id) = self::escape($sessionId);
        return !!self::query("DELETE FROM session WHERE ss_id='$id'");
    }

    public function close()
    {
        return true;
    }
}
