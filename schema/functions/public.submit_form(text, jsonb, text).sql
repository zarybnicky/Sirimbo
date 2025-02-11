CREATE FUNCTION public.submit_form(type text, data jsonb, url text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  v_email text;
begin
  insert into form_responses (type, data, url) values (type, data, url);

  if current_tenant_id() = 1 then
    foreach v_email in array (array['m.hyzova96@seznam.cz', 'miroslav.hyza@tkolymp.cz', 'hyzam@tkolymp.cz', 'filip.karasek@tkolymp.cz']) loop
      perform graphile_worker.add_job(
        'send_email',
        json_build_object(
          'template', 'notify_submitted_form.mjml',
          'options', json_build_object(
          'to', v_email,
          'subject', 'Nový vyplněný formulář z webu'
        ),
        'variables', json_build_object(
          'url', url,
          'data', data
        )
      ));
    end loop;
  end if;
end;
$$;

GRANT ALL ON FUNCTION public.submit_form(type text, data jsonb, url text) TO anonymous;
