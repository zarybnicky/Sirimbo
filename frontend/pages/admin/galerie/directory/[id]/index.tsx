import * as React from 'react';
import { useGalleryDirQuery, useDeleteGalleryPhotoMutation } from 'lib/graphql/Gallery';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useRouter } from 'next/router';
import { DataGrid, GridActionsCellItem } from '@mui/x-data-grid';
import { Edit as EditIcon } from 'react-feather';
import { DeleteButton } from 'components/DeleteButton';

export default function AdminGalleryPhotoList() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { data, refetch } = useGalleryDirQuery({ id: router.query.id as string });
  const { mutateAsync: doDelete } = useDeleteGalleryPhotoMutation({
    onSuccess: () => refetch(),
  });

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <DataGrid
      autoHeight={true}
      getRowId={row => row.gfId}
      rows={data?.galerieDir?.galerieFotosByGfIdRodic.nodes || []}
      columns={[
        {
          field: 'actions',
          type: 'actions',
          getActions: ({ id }) => [
            <GridActionsCellItem key="edit"
              icon={<EditIcon />}
              onClick={() => router.push(`/admin/galerie/file/edit/${id}`)}
              label="Upravit"
            />,
            <DeleteButton key="del" onDelete={doDelete} id={id} title="smazat fotku" />,
          ]
        },
        { field: 'gfName', headerName: 'Jméno', flex: 1 },
      ]}
    />
  </div>;
}
