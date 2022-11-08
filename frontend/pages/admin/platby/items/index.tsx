import { useRouter } from 'next/router';
import { NextLinkComposed } from "components/Link";
import { usePaymentItemListQuery, useDeletePaymentItemMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';
import format from 'date-fns/format';

export default function PlatbyItemListPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = usePaymentItemListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentItemMutation({
    onSuccess: () => refetch(),
  });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/platby/items/add" className="btn btn-primary">
      Nová platba
    </NextLinkComposed>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.piId}
      rows={data?.platbyItems?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/items/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton
              key="delete" title="smazat platbu"
              params={{ id: id.toString() }} onDelete={doDelete}
            />,
          ]
        },
        {
          field: 'piAmount', headerName: 'Částka', flex: 1,
          valueFormatter: ({ value }) => `${value} Kč`,
        },
        {
          field: 'piDate', headerName: 'Datum', flex: 1,
          valueFormatter: ({ value }) => format(new Date(value), 'd. M. y'),
        },
        {
          field: 'piIdUser', headerName: 'Uživatel', flex: 1,
          valueGetter: ({ row }) => `${row.userByPiIdUser?.uJmeno} ${row.userByPiIdUser?.uPrijmeni}`,
        },
        {
          field: 'piIdCategory', headerName: 'Kategorie', flex: 1,
          valueGetter: ({ row }) => `${row.platbyCategoryByPiIdCategory?.pcName}`,
        },
      ]}
    />
  </Container>;
}
