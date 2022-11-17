import React from "react";
import { useConfirm } from 'components/Confirm';
import { Trash2 as DeleteIcon } from 'react-feather';
import { toast } from 'react-toastify';
import classNames from "classnames";

export const DeleteButton = React.memo(function DeleteButton({
  title, onDelete
}: {
  title: string;
  onDelete: () => Promise<unknown>;
}): React.ReactElement | null {
  const confirm = useConfirm();

  const deleteItem = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat ${title}?` });
    try {
      await onDelete();
      toast.success('Smazáno');
    } catch (e) {
      if (e instanceof Error) {
        toast.error(e.message);
      } else {
        toast.error('Nepodařilo se smazat položku');
      }
    }
  }, [confirm, onDelete, title]);

  return <button onClick={deleteItem} className={classNames(
    "flex gap-1 items-center relative shadow-md text-xs font-bold pl-2 pr-3 py-1 rounded-2xl",
    'bg-white hover:bg-stone-50',
  )}>
    <DeleteIcon className="w-4" /> Odstranit
  </button>;
});
