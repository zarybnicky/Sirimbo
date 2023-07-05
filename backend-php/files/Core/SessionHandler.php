<?php
class DbSessionHandler extends Database implements SessionHandlerInterface
{
    public function open(string $savePath, string $sessionName): bool
    {
        return true;
    }

    public function read(string $sessionId): string | false
    {
        $stmt = self::prepare(
            "SELECT * FROM session WHERE ss_id=? AND (extract(epoch from ss_updated_at) + ss_lifetime) > extract(epoch from now())",
        );
        $stmt->execute([$sessionId]);
        if (!($result = self::getSingleRow($stmt))) {
            return '';
        }
        return serialize(["id" => $result['ss_user']]);
    }

    public function write(string $sessionId, string $data): bool
    {
        setcookie(session_name(), $sessionId, time() + 86400, '/');
        $sessionData = unserialize($data);
        $userId = $sessionData['id'] ?? null;
        $stmt = self::prepare(
            "INSERT INTO session
             (ss_id, ss_user, ss_lifetime) VALUES
             (?, ?, 86400)
             ON CONFLICT (ss_id) DO UPDATE SET
             ss_user=EXCLUDED.ss_user, ss_updated_at=NOW()"
        );
        $stmt->bindParam(1, $sessionId);
        $stmt->bindParam(2, $userId);
        $stmt->execute();
        return !!$stmt;
    }

    public function gc(int $maxLifetime): int | false
    {
        $stmt = self::prepare(
            "DELETE FROM session WHERE (extract(epoch from ss_updated_at) + ss_lifetime) < extract(epoch from now())"
        );
        return $stmt->execute() ? 0 : false;
    }

    public function destroy(string $sessionId): bool
    {
        setcookie(session_name(), '', -1, '/');
        $stmt = self::prepare("DELETE FROM session WHERE ss_id=?");
        return !!$stmt->execute([$sessionId]);
    }

    public function close(): bool
    {
        return true;
    }
}
