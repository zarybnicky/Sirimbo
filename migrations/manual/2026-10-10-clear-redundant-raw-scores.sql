update federated.judge_score
set raw_score = null
where raw_score is not null
  and case
    when component = 'mark' and raw_score in ('x', 'mark') then score = 1
    when component = 'mark' and raw_score in ('-', 'set=false') then score = 0
    when raw_score ~ '^[+-]?[0-9]+([.][0-9]+)?$' then raw_score::numeric = score
    else false
  end;
