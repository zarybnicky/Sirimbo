import * as React from 'react';
import { useCoupleListQuery, useDeleteCoupleMutation, useFixUnpairedCouplesMutation } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { DataGrid } from '@mui/x-data-grid';
import { DeleteButton } from 'components/DeleteButton';
import { NewCoupleForm } from 'components/NewCoupleForm';
import { Dialog, DialogContent, DialogTitle } from '@mui/material';
import { useSnackbar } from 'notistack';

export default function CoupleAdminList() {
  useRequireUserLoggedIn();
  const { enqueueSnackbar } = useSnackbar();
  const { data, refetch } = useCoupleListQuery();
  const { mutateAsync: doDelete } = useDeleteCoupleMutation({
    onSuccess: () => refetch(),
  });
  const { mutateAsync: doFix } = useFixUnpairedCouplesMutation({
    onSuccess: () => refetch(),
  });

  const fix = React.useCallback(async () => {
    const data = await doFix({});
    enqueueSnackbar(`Opraveno ${data.fixUnpairedCouples?.paries?.length || 0} záznamů`);
  }, [doFix, enqueueSnackbar]);

  const [open, setOpen] = React.useState(false);

  return <div className="container mx-auto max-w-5xl" style={{ margin: '4rem auto 6rem' }}>
    <button className="button button-text button-red" onClick={() => setOpen(true)}>Nový pár</button>
    <button className="button button-text button-red" onClick={fix}>Opravit nespárované páry</button>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.pId}
      rows={data?.activeCouples?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat pár" />,
          ]
        },
        {
          field: 'atJmeno', headerName: 'Jméno', flex: 1,
          valueGetter: ({ row }) => (
            `${row.userByPIdPartner?.uPrijmeni}, ${row.userByPIdPartner?.uJmeno}` +
            (row.userByPIdPartnerka ? ` - ${row.userByPIdPartnerka.uPrijmeni}, ${row.userByPIdPartnerka.uJmeno}` : '')
          ),
        },
      ]}
    />

    <Dialog onClose={() => setOpen(false)} open={open}>
      <DialogTitle>Nový pár</DialogTitle>
      <DialogContent>
        <NewCoupleForm onSuccess={() => { refetch(); setOpen(false) }} />
      </DialogContent>
    </Dialog>
  </div>;
}
