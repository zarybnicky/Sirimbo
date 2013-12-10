%h1
    Správa akcí

!= $notice

%form.centerWTable(action="" method="POST")
    %button(type="submit" name="action" value="enter" style="display:none;")
        Enter

    != $header

    %table
        %tr
            %td
                Uživatel
            %td

        - $userselect = (new TKOlomouc\View\Helper\UserSelect())->users($users);
        - foreach ($items as $item)
            %tr
                %td
                    - if ($item['ai_user'] > 0)
                        - $this->post($item['ai_id'] . '-user', $item['ai_user']);
                    != $userselect->name($item['ai_id'] . '-user');
                %td
                    %button(type="submit" name="remove" value="#{$item['ai_id']}")
                        Odstranit
        %tr
            %td
                != $userselect->name('add-user'); ?></td>
            %td
                %button(type="submit" name="add" value="add")
                    Přidat
    %button(type="submit" name="action" value="submit")
        Uložit
    %a(href="/admin/akce")
        Zpět
