import React from 'react';
import { DeletePersonDocument, PersonMembershipsDocument } from '@/graphql/Person';
import { TitleBar } from '@/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@/ui/use-auth';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { formatAgeGroup } from '@/ui/format';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { useRouter } from 'next/router';
import { UserCheck2, UserX2 } from 'lucide-react';
import { PersonAccessView } from '@/ui/PersonAccessView';
import { PersonMembershipView } from '@/ui/PersonMembershipView';
import { PersonAttendanceView } from '@/ui/PersonAttendanceView';
import { PersonPaymentsView } from '@/ui/PersonPaymentsView';

export function PersonView({ id }: { id: string }) {
  const auth = useAuth();
  const confirm = useConfirm();

  const router = useRouter();
  const [{ data }] = useQuery({ query: PersonMembershipsDocument, variables: { id }, pause: !id });
  const [tab, setTab] = useQueryParam('tab', StringParam);

  const isAdminOrCurrentPerson = auth.isAdmin || auth.personIds.some(x => x  === id);
  const item = data?.person;

  const doRemove = useMutation(DeletePersonDocument)[1];
  const remove = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item?.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!` });
    await doRemove({ id })
    router.replace('/clenove')
  }, [id, router, item, confirm, doRemove]);

  const tabs = React.useMemo(() => {
    if (!item) return [];

    const tabs = [
      {
        id: 'info',
        label: <>Členství</>,
        contents: () => <PersonMembershipView key="memberships" item={item} />,
      }
    ];
    if (isAdminOrCurrentPerson) {
      tabs.push({
        id: 'events',
        label: <>Účasti</>,
        contents: () => <PersonAttendanceView id={id} />,
      });
      tabs.push({
        id: 'payment',
        label: <>Platby</>,
        contents: () => <PersonPaymentsView key="payments" id={id} />,
      });
      tabs.push({
        id: 'access',
        label: <>Přístupy {item.userProxiesList.length > 0 ? <UserCheck2 /> : <UserX2 />}</>,
        contents: () => <PersonAccessView key="access" item={item} />,
      });
    }
    return tabs;
  }, [id, item, isAdminOrCurrentPerson]);

  if (!item) return null

  return (
    <>
      <TitleBar title={item.name}>
        {isAdminOrCurrentPerson && (
          <Dialog>
            <DialogTrigger.Edit size="sm" />
            <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
              <EditPersonForm data={item} />
            </DialogContent>
          </Dialog>
        )}

        {auth.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTrigger.CornerDots className="relative top-0 right-0" />
            <DropdownMenuContent align="end">
              <DropdownMenuButton onClick={remove}>
                Smazat
              </DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </TitleBar>

      <dl className="mb-2">
        {item.birthDate && (
          <>
            <dt>Věková kategorie</dt>
            <dd>{formatAgeGroup(item)}</dd>
          </>
        )}
        {item.phone && (
          <>
            <dt>Telefon</dt>
            <dd>{item.phone}</dd>
          </>
        )}
        {item.email && (
          <>
            <dt>E-mail</dt>
            <dd>{item.email}</dd>
          </>
        )}
      </dl>

      <TabMenu selected={tab || tabs[0]?.id} onSelect={setTab} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === tab) || tabs[0])?.contents()}
      </div>
    </>
  );
}
