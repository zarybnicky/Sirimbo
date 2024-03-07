import { DeleteEventInstanceDocument, EventInstanceWithEventFragment } from "@/graphql/Event";
import React from "react";
import { useMutation } from "urql";
import { useConfirm } from "./Confirm";
import { cn } from "./cn";
import { useAuth } from "./use-auth";
import { Trash2 } from "lucide-react";

export function DeleteInstanceButton({ instance, className }: {
  instance: EventInstanceWithEventFragment;
  className?: string;
}) {
  const { perms } = useAuth();
  const confirm = useConfirm();
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];

  const deleteInstance = React.useCallback(async () => {
    if ((instance.event?.eventInstancesList.length ?? 0) < 2) {
      await confirm({ description: 'Opravdu chcete smazat CELOU UDÁLOST? Smažete tím všechny záznamy o účasti i platbách.' });
    } else {
      await confirm({ description: 'Opravdu chcete smazat JEDEN TERMÍN události? Smažete tím všechny záznamy o účasti i platbách.' });
    }
    await deleteMutation({ id: instance.id });
  }, [confirm, instance, deleteMutation]);

  if (perms.isAdmin || (perms.isTrainer && instance.event?.eventTrainersList.find(x => perms.isCurrentPerson(x.person?.id)))) {
    return (
      <button
        type="button"
        className={cn("rounded-sm opacity-70 ring-offset-neutral-7 transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-accent-7 focus:ring-offset-2 disabled:pointer-events-none data-[state=open]:bg-accent-5 data-[state=open]:text-white", className)}
        onClick={deleteInstance}
      >
        <Trash2 className="w-4 h-4" />
      </button>
    );
  }
  return null;
}
