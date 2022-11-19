import * as React from 'react';
import { useCoupleListQuery, useDeleteCoupleMutation, useFixUnpairedCouplesMutation } from 'lib/graphql/Couple';
import { DeleteButton } from 'components/DeleteButton';
import { NewCoupleForm } from 'components/NewCoupleForm';
import { toast } from 'react-toastify';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';
import { Item } from 'components/layout/Item';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function CoupleAdminList() {
  const { data, refetch } = useCoupleListQuery();
  const { mutateAsync: doDelete } = useDeleteCoupleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doFix } = useFixUnpairedCouplesMutation({
    onSuccess: () => refetch(),
  });

  const fix = React.useCallback(async () => {
    const data = await doFix({});
    toast.info(`Opraveno ${data.fixUnpairedCouples?.paries?.length || 0} záznamů`);
  }, [doFix]);

  return <Item>
    <button className="button button-red" onClick={fix}>Opravit nespárované páry</button>
    <SimpleDialog
      title="Nový pár"
      button={<button className="button button-red">Nový pár</button>}
    >
      {({ close }) => <NewCoupleForm onSuccess={() => { refetch(); close(); }} />}
    </SimpleDialog>

    {data?.activeCouples?.nodes.map((row) => (
      <Card key={row.id}>
        <DeleteButton key="del" onDelete={() => doDelete({ id: row.id })} title="smazat pár" />
        {row.userByPIdPartner?.uPrijmeni}, {row.userByPIdPartner?.uJmeno}
        {(row.userByPIdPartnerka ? ` - ${row.userByPIdPartnerka.uPrijmeni}, ${row.userByPIdPartnerka.uJmeno}` : '')}
      </Card>
    ))}
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePary, PermissionLevel.P_OWNED,
);
