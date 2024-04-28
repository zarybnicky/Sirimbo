import React from 'react';
import { DeletePersonDocument, PersonMembershipsDocument } from '@/graphql/Person';
import { TitleBar } from '@/ui/TitleBar';
import { useMutation, useQuery } from 'urql';
import { useAuth } from '@/ui/use-auth';
import { EditPersonForm } from '@/ui/EditPersonForm';
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
import { buttonCls } from '@/ui/style';

export function PersonView({ id }: { id: string }) {
  const auth = useAuth();
  const isAdminOrCurrentPerson = auth.isAdmin || auth.personIds.some(x => x  === id);

  const router = useRouter();
  const [{ data }] = useQuery({ query: PersonMembershipsDocument, variables: { id }, pause: !id });
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
          <Dialog open={editOpen} onOpenChange={setEditOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
                Upravit
              </button>
            </DialogTrigger>
            <DialogContent className="sm:max-w-2xl" onPointerDownOutside={(e) => e.preventDefault()}>
              <EditPersonForm data={item} onSuccess={() => setEditOpen(false)} />
            </DialogContent>
          </Dialog>
        )}

        {auth.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTriggerDots className="relative top-0 right-0" />
            <DropdownMenuContent align="end">
              <DropdownMenuButton
                onClick={async () => {
                  await confirm({ description: `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!` });
                  await deleteMutation({ id })
                  router.replace('/clenove')
                }}
              >
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

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents()}
      </div>
    </>
  );
}
