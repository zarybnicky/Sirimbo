import React from "react";
import { useSnackbar } from 'notistack';
import { useConfirm } from 'material-ui-confirm';
import DeleteIcon from '@mui/icons-material/Delete';
import { GridActionsCellItem } from '@mui/x-data-grid';

const genericMemo: <T>(component: T) => T = React.memo

export const DeleteButton = genericMemo(function DeleteButton<T>({
  title, params, onDelete
}: {
  title: string;
  params: T;
  onDelete: (x: T) => Promise<unknown>;
}): React.ReactElement | null {
  const confirm = useConfirm();
  const { enqueueSnackbar } = useSnackbar();

  const deleteItem = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat ${title}?` });
    try {
      await onDelete(params);
      enqueueSnackbar('Smazáno', { variant: 'success' });
    } catch (e) {
      if (e instanceof Error) {
        enqueueSnackbar(e.message, { variant: 'error' });
      } else {
        enqueueSnackbar('Nepodařilo se smazat položku', { variant: 'error' });
      }
    }
  }, [])

  return <GridActionsCellItem key="delete" icon={<DeleteIcon />} onClick={deleteItem} label="Odstranit" />;
});
