import { CohortListDocument } from '@app/graphql/Cohorts';
import { UserFragment } from '@app/graphql/CurrentUser';
import { RoleListDocument } from '@app/graphql/Roles';
import {ConfirmUserDocument, DeleteUserDocument, UserListDocument,} from '@app/graphql/User';
import { Card } from '@app/ui/Card';
import { ComboboxElement } from '@app/ui/Combobox';
import { useConfirm } from '@app/ui/Confirm';
import { TitleBar } from '@app/ui/TitleBar';
import { UserList } from '@app/ui/UserList';
import { fullDateFormatter } from '@app/ui/format-date';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { zodResolver } from '@hookform/resolvers/zod';
import { Check as CheckIcon, Trash2 as DeleteIcon } from 'lucide-react';
import { NextPageWithLayout } from 'pages/_app';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  cohort: z.string(),
  role: z.string(),
});
type FormProps = z.infer<typeof Form>;

const UnconfirmedUser: React.FC<{
  item: UserFragment;
  onProcessed: () => void;
}> = ({ item, onProcessed }) => {
  const confirm = useConfirm();
  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: { cohort: item.uSkupina },
    resolver: zodResolver(Form),
  });

  const [{ data: cohorts }] = useQuery({ query: CohortListDocument });
  const [{ data: roles }] = useQuery({ query: RoleListDocument });
  const confirmUser = useMutation(ConfirmUserDocument)[1];
  const deleteUser = useMutation(DeleteUserDocument)[1];

  const onSubmit = React.useCallback(
    async (values: FormProps) => {
      await confirmUser({ id: item.id, cohort: values.cohort, role: values.role });
      onProcessed();
    },
    [item, onProcessed, confirmUser],
  );

  const onDelete = React.useCallback(async () => {
    try {
      await confirm({ description: `Vymazat uživatele ${item.uLogin}?` });
      await deleteUser({ id: item.id });
      onProcessed();
    } catch {}
  }, [item, confirm, deleteUser, onProcessed]);

  return (
    <Card className="mb-8">
      <form className="grid md:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit)}>
        <div>
          <h5 className="text-xl font-bold mb-2">
            {item.uJmeno} {item.uPrijmeni}
          </h5>
          <div>
            <b>Login:</b> {item.uLogin}
          </div>
          <div>
            <b>Datum narození:</b> {fullDateFormatter.format(new Date(item.uNarozeni))}
          </div>
          <div>
            <b>E-mail:</b> {item.uEmail}
          </div>
          <div>
            <b>Telefon:</b> {item.uTelefon}
          </div>
          <div>
            <b>Poznámka:</b> {item.uPoznamky}
          </div>
        </div>

        <div className="flex gap-2">
          <ComboboxElement
            control={control}
            name="cohort"
            label="Tréninková skupina"
            placeholder="vyberte skupinu"
            options={(cohorts?.skupinies?.nodes || []).map((item) => ({
              id: item.id,
              label: item.sName,
            }))}
          />
          <ComboboxElement
            control={control}
            name="role"
            label="Role oprávnění"
            placeholder="vyberte roli"
            options={roles?.permissions?.nodes?.map((x) => ({
              id: x.id,
              label: x.peName,
            }))}
          />
        </div>

        <div className="col-full flex gap-4 flex-row-reverse">
          <button type="submit" className="button button-accent flex gap-2 items-center">
            <CheckIcon /> Potvrdit
          </button>
          <button
            type="button"
            onClick={onDelete}
            className="button button-accent flex gap-2 items-center"
          >
            <DeleteIcon /> Odstranit
          </button>
        </div>
      </form>
    </Card>
  );
};

const Page: NextPageWithLayout = () => {
  const [{ data: users }, refetch] = useQuery({
    query: UserListDocument,
    variables: { confirmed: false },
  });

  return (
    <div className="container py-4">
      <TitleBar backHref="/admin/users" title="Nepotvrzení uživatelé" />
      {users?.users?.nodes?.map((item, i) => (
        <UnconfirmedUser onProcessed={refetch} item={item} key={i} />
      ))}
    </div>
  );
};

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = 'Uživatelé';

export default Page;
