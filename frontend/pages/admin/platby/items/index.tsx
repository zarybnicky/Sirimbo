import { useRouter } from 'next/router';
import { usePaymentItemListQuery, useDeletePaymentItemMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { format } from 'date-fns';
import { Button } from 'components/Button';

export default function PlatbyItemListPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = usePaymentItemListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentItemMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/platby/items/add">Nová platba</Button>

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
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat platbu" />,
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
  </div>;
}
