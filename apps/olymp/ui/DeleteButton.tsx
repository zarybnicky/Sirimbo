import { useConfirm } from '@app/ui/Confirm';
import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { Trash2 as DeleteIcon } from 'lucide-react';
import { LinkProps } from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

type DeleteButtonProps = {
  title: string;
  doc: TypedDocumentNode<any, { id: string }>;
  id: string;
  redirect?: LinkProps['href'];
};

export const DeleteButton = React.memo(function DeleteButton({
  title,
  doc,
  id,
  redirect,
}: DeleteButtonProps) {
  const router = useRouter();
  const confirm = useConfirm();
  const deleteMutation = useMutation(doc)[1];

  const deleteItem = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete smazat ${title}?` });
    try {
      await deleteMutation({ id });
      toast.success('Smazáno');
      if (redirect) {
        router.push(redirect);
      }
    } catch (e) {
      if (e instanceof Error) {
        toast.error(e.message);
      } else {
        toast.error('Nepodařilo se smazat položku');
      }
    }
  }, [deleteMutation, id, confirm, title, redirect, router]);

  return (
    <button
      onClick={deleteItem}
      className="shadow-md bg-red-500 hover:bg-red-600 text-white flex items-center gap-1 px-3 rounded-2xl py-1 text-xs tracking-tight font-bold"
    >
      <DeleteIcon className="w-4" /> Odstranit
    </button>
  );
});
