import React from 'react';
import { DeletePersonDocument, PersonDocument } from '@app/graphql/Person';
import { TitleBar } from '@app/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@app/ui/use-auth';
import { EditPersonForm } from '@app/ui/EditPersonForm';
import { formatAgeGroup } from '@/ui/format';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { useConfirm } from './Confirm';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { useRouter } from 'next/router';
import { UserCheck2, UserX2 } from 'lucide-react';
import { PersonAccessView } from './PersonAccessView';
import { PersonMembershipView } from './PersonMembershipView';
import { PersonAttendanceView } from './PersonAttendanceView';
import { PersonPaymentsView } from './PersonPaymentsView';

export function PersonView({ id }: { id: string }) {
  const { perms } = useAuth();
  const router = useRouter();
  const [{ data }] = useQuery({ query: PersonDocument, variables: { id }, pause: !id });
  const [variant, setVariant] = useQueryParam('tab', StringParam);
  const confirm = useConfirm();
  const deleteMutation = useMutation(DeletePersonDocument)[1];
  const [editOpen, setEditOpen] = React.useState(false);

  const item = data?.person;
  const tabs = React.useMemo(() => {
    if (!item) return [];

    const tabs = [
      {
        id: 'info',
        label: <>Členství</>,
        contents: () => <PersonMembershipView key="memberships" item={item} />,
      }
    ];
    if (!!item.eventAttendancesList?.length) {
      tabs.push({
        id: 'events',
        label: <>Účasti</>,
        contents: () => <PersonAttendanceView item={item} />,
      });
    }
    if (perms.isAdmin || perms.isCurrentPerson(item.id)) {
      tabs.push({
        id: 'payment',
        label: <>Platby</>,
        contents: () => <PersonPaymentsView key="payments" item={item} />,
      });
      tabs.push({
        id: 'access',
        label: <>Přístupy {item.userProxiesList.length > 0 ? <UserCheck2 /> : <UserX2 />}</>,
        contents: () => <PersonAccessView key="access" item={item} />,
      });
    }
    return tabs;
  }, [item, perms]);

  if (!item) return null

  return (
    <>
      <TitleBar title={item.name}>
        {(perms.isAdmin || perms.isCurrentPerson(item.id)) && (
          <DropdownMenu>
            <DropdownMenuTriggerDots />
            <DropdownMenuContent align="end">
              <Dialog open={editOpen} onOpenChange={setEditOpen}>
                <DialogTrigger asChild>
                  <DropdownMenuButton onSelect={(e) => e.preventDefault()}>
                    Upravit
                  </DropdownMenuButton>
                </DialogTrigger>
                <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
                  <EditPersonForm data={item} onSuccess={() => setEditOpen(false)} />
                </DialogContent>
              </Dialog>

              {perms.isAdmin && !perms.isCurrentPerson(item.id)  && (
                <DropdownMenuButton
                  onClick={async () => {
                    await confirm({ description: `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!` });
                    await deleteMutation({ id })
                    router.replace('/clenove')
                  }}
                >
                  Smazat
                </DropdownMenuButton>
              )}
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

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents()}
      </div>
    </>
  );
}
