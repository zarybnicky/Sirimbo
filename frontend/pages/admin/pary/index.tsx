import * as React from 'react';
import { useCoupleListQuery, useDeleteCoupleMutation, useFixUnpairedCouplesMutation } from 'lib/graphql/Couple';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DeleteButton } from 'components/DeleteButton';
import { NewCoupleForm } from 'components/NewCoupleForm';
import { toast } from 'react-toastify';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';

export default function CoupleAdminList() {
  useRequireUserLoggedIn();
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

CoupleAdminList.getLayout = (page: React.ReactElement) => <Layout>{page}</Layout>;
