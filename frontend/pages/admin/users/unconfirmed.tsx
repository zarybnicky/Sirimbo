import { Card } from 'components/Card';
import { useDeleteUserMutation, useConfirmUserMutation, useUserListQuery } from 'lib/graphql/User';
import { SelectElement } from 'components/SelectElement';
import { useForm } from "react-hook-form";
import { Trash2 as DeleteIcon, Check as CheckIcon } from 'react-feather';
import { useConfirm } from 'components/Confirm';
import React from "react";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { UserFragment } from 'lib/graphql/CurrentUser';
import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { useRoleListQuery } from 'lib/graphql/Roles';
import { formatFullDate } from 'lib/format-date';

const UnconfirmedUser: React.FC<{
  item: UserFragment;
  onProcessed: () => void;
}> = ({ item, onProcessed }) => {
  const confirm = useConfirm();
  const { data: cohorts } = useCohortListQuery({ visible: undefined });
  const { data: roles } = useRoleListQuery();
  const { control, handleSubmit } = useForm({
    defaultValues: {
      cohort: item.uSkupina,
      role: undefined as number | undefined,
    },
  });

  const { mutateAsync: confirmUser } = useConfirmUserMutation();
  const { mutateAsync: deleteUser } = useDeleteUserMutation();

  const onSubmit = React.useCallback(async (values) => {
    await confirmUser({ id: item.id, cohort: values.cohort, role: values.role });
    onProcessed();
  }, [item, onProcessed, confirmUser]);

  const onDelete = React.useCallback(async () => {
    try {
      await confirm({ description: `Vymazat uživatele ${item.uLogin}?` })
      await deleteUser({ id: item.id });
      onProcessed();
    } catch { }
  }, [item, confirm, deleteUser, onProcessed]);

  return (
    <Card className="mb-8">
      <form className="grid md:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit)}>
        <div>
          <h5 className="text-xl font-bold mb-2">{item.uJmeno} {item.uPrijmeni}</h5>
          <div>
            <b>Login:</b> {item.uLogin}
          </div>
          <div>
            <b>Datum narození:</b> {formatFullDate(new Date(item.uNarozeni))}
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
          <SelectElement
            required control={control} name="cohort" label="Tréninková skupina"
            options={(cohorts?.skupinies?.nodes || []).map(item => ({ id: item.id, label: item.sName }))}
          />
          <SelectElement
            required control={control} name="role" label="Role oprávnění"
            options={(roles?.permissions?.nodes || []).map(item => ({ id: item.id, label: item.peName }))}
          />
        </div>

        <div className="col-full flex gap-4 flex-row-reverse">
          <button type="submit" className="button button-red button-text flex gap-2 items-center">
            <CheckIcon /> Potvrdit
          </button>
          <button type="button" onClick={onDelete} className="button button-red button-text flex gap-2 items-center">
            <DeleteIcon /> Odstranit
          </button>
        </div>
      </form>
    </Card>
  );
};

export default function UnconfirmedUsers() {
  useRequireUserLoggedIn();
  const { data: users, refetch } = useUserListQuery({ confirmed: false });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <h4 className="text-xl font-bold">Nepotvrzení uživatelé</h4>
    {users?.users?.nodes?.map((item, i) => <UnconfirmedUser onProcessed={refetch} item={item} key={i} />)}
  </div>;
};
