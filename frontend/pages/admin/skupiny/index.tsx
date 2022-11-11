import { useRouter } from 'next/router';
import { useCohortListQuery, useDeleteCohortMutation } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { DataGrid, GridActionsCellItem, GridRowParams } from '@mui/x-data-grid';
import EditIcon from '@mui/icons-material/Edit';
import { DeleteButton } from 'components/DeleteButton';
import { Container } from '@mui/material';
import { Button } from 'components/Button';

export default function CohortsPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useCohortListQuery();
  const { mutateAsync: doDelete } = useDeleteCohortMutation({
    onSuccess: () => refetch(),
  });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/skupiny/add">Nová skupina</Button>

    <DataGrid
      autoHeight={true}
      getRowId={row => row.sId}
      rows={data?.skupinies?.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }: GridRowParams) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/skupiny/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat skupinu" />,
          ]
        },
        {
          field: 'sName', headerName: 'Jméno', flex: 1,
          renderCell: ({ row }) => <>
            <div className="w-3.5 h-3.5" title={row.sDescription} style={{ backgroundColor: row.sColorRgb }} />
            {' '}{row.sName}
          </>,
        },
        { field: 'sLocation', headerName: 'Místo', flex: 1 },
        {
          field: 'sVisible', headerName: 'Viditelná', flex: 1,
          valueFormatter: ({ value }) => value ? '' : 'skrytá',
        },
      ]}
    />
  </Container>;
}
