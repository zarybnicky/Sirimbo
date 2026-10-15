WITH compared AS (
  SELECT p.id AS payment_id, p.tenant_id, i.id AS instance_id, i.since,
    i.name, i.is_cancelled, p.status, p.is_auto_credit_allowed,
    ARRAY(SELECT d.person_id FROM payment_debtor d
      WHERE d.payment_id = p.id ORDER BY d.person_id) AS charged_people,
    ARRAY(SELECT DISTINCT r.person_id FROM event_instance_registration r
      WHERE r.instance_id = i.id AND r.registration_status = 'active'
        AND r.person_id IS NOT NULL AND NOT i.is_cancelled
      ORDER BY r.person_id) AS expected_people,
    (SELECT sum(r.amount) FROM payment_recipient r WHERE r.payment_id = p.id) AS lesson_price,
    (SELECT count(*) FROM transaction t WHERE t.payment_id = p.id) AS transaction_count,
    (SELECT count(*) FROM transaction t
      WHERE t.payment_id = p.id AND t.source = 'auto-credit') AS auto_credit_count
  FROM payment p
  JOIN event_instance i ON i.id = p.event_instance_id
  WHERE p.tenant_id = 2 AND i.type = 'lesson'
)
SELECT *,
  ARRAY(SELECT unnest(charged_people) EXCEPT SELECT unnest(expected_people)) AS remove_people,
  ARRAY(SELECT unnest(expected_people) EXCEPT SELECT unnest(charged_people)) AS add_people
FROM compared
WHERE charged_people IS DISTINCT FROM expected_people
ORDER BY since, payment_id;
