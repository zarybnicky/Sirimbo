CREATE FUNCTION federated.class_rank(class text) RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$
  select case class
    when 'M' then 16
    when 'S' then 16
    when 'A' then 15
    when 'B' then 14
    when 'C' then 13
    when 'D' then 12
    when 'E' then 11
    when 'Gold' then 4
    when 'Silver' then 3
    when 'Bronze' then 2
    when 'Novice' then 1
    else 0
  end;
$$;
