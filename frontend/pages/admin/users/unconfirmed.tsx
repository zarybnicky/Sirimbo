import { Card } from 'components/Card';
import {
  ConfirmUserDocument,
  DeleteUserDocument,
  UserListDocument,
} from '@app/graphql/User';
import { ComboboxElement } from 'components/Combobox';
import { useForm } from 'react-hook-form';
import { Trash2 as DeleteIcon, Check as CheckIcon } from 'lucide-react';
import { useConfirm } from 'components/Confirm';
import React from 'react';
import { UserFragment } from '@app/graphql/CurrentUser';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { RoleListDocument } from '@app/graphql/Roles';
import { fullDateFormatter } from 'lib/format-date';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { UserList } from 'components/UserList';
import { NextPageWithLayout } from 'pages/_app';
import { useMutation, useQuery } from 'urql';
import { TitleBar } from 'components/layout/TitleBar';

type FormProps = {
  cohort: string;
  role: string;
};

const UnconfirmedUser: React.FC<{
  item: UserFragment;
  onProcessed: () => void;
}> = ({ item, onProcessed }) => {
  const confirm = useConfirm();
  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: { cohort: item.uSkupina },
  });

  const [{ data: cohorts }] = useQuery({query: CohortListDocument});
  const [{ data: roles }] = useQuery({query: RoleListDocument});
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
            required
            control={control}
            name="cohort"
            label="Tréninková skupina"
            options={(cohorts?.skupinies?.nodes || []).map((item) => ({
              id: item.id,
              label: item.sName,
            }))}
          />
          <ComboboxElement
            required
            control={control}
            name="role"
            label="Role oprávnění"
            options={roles?.permissions?.nodes?.map((x) => ({
              id: x.id,
              label: x.peName,
            }))}
          />
        </div>

        <div className="col-full flex gap-4 flex-row-reverse">
          <button
            type="submit"
            className="button button-accent flex gap-2 items-center"
          >
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
  const [{data: users}, refetch] = useQuery({query: UserListDocument, variables:{ confirmed: false }});

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
