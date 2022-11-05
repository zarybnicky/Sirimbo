import { useRouter } from 'next/router';
import { NextLinkComposed } from "components/Link";
import { usePaymentGroupListQuery, useDeletePaymentGroupMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';

export default function PlatbyGroupListPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = usePaymentGroupListQuery();
  const { mutateAsync: doDelete } = useDeletePaymentGroupMutation({
    onSuccess: () => refetch(),
  });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <NextLinkComposed href="/admin/platby/structure/group/add" className="btn btn-primary">
      Nová skupina plateb
    </NextLinkComposed>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.pgId}
      rows={data?.platbyGroups?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/platby/structure/group/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton
              key="delete" title="smazat skupinu plateb"
              params={{ id: id.toString() }} onDelete={doDelete}
            />,
          ]
        },
        { field: 'pgName', headerName: 'Jméno', flex: 1 },
        {
          field: 'pgType', headerName: 'Typ', flex: 1,
          renderCell: ({ row }) => row.pgType ? 'Členské příspěvky' : 'Běžné platby',
        },
      ]}
    />
  </Container>;
}
