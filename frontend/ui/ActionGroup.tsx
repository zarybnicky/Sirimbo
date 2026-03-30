import React, { useState } from 'react';
import { Dialog, DialogContent } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { useConfirm } from '@/ui/Confirm';
import type { ResolvedActions, ResolvedAction } from '@/lib/actions';
import { buttonCls } from '@/ui/style';

export function ActionGroup<TItem>({ actions }: { actions: ResolvedActions<TItem> }) {
  const { primary, secondary, ctx } = actions;
  const confirm = useConfirm();
  const [dialogAction, setDialogAction] = useState<ResolvedAction<TItem> | null>(null);
  const [pending, setPending] = useState<string | null>(null);

  if (primary.length === 0 && secondary.length === 0) return null;

  const trigger = async (action: ResolvedAction<TItem>) => {
    if (action.type === 'dialog') {
      setDialogAction(action);
      return;
    }

    if (action.confirm) {
      try {
        await confirm({ description: action.confirm });
      } catch {
        return;
      }
    }

    setPending(action.id);
    try {
      await action.execute(ctx);
    } finally {
      setPending(null);
    }
  };

  return (
    <>
      <div className="flex items-center gap-2">
        {primary.map((action) => (
          <button
            key={action.id}
            type="button"
            disabled={pending === action.id}
            className={buttonCls({ variant: 'outline', size: 'sm' })}
            onClick={() => trigger(action)}
          >
            {action.icon && <action.icon className="size-4" />}
            {action.label}
          </button>
        ))}

        {secondary.length > 0 && (
          <DropdownMenu>
            <DropdownMenuTrigger.CornerDots className="relative top-0 right-0" />
            <DropdownMenuContent align="end">
              {secondary.map((action) => (
                <DropdownMenuButton
                  key={action.id}
                  disabled={pending === action.id}
                  onSelect={() => trigger(action)}
                  className={action.variant === 'danger' ? 'bg-accent-3/70' : undefined}
                >
                  {action.icon && <action.icon className="size-4" />}
                  {action.label}
                </DropdownMenuButton>
              ))}
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </div>

      {dialogAction && dialogAction.type === 'dialog' && (
        <Dialog open onOpenChange={() => setDialogAction(null)}>
          <DialogContent {...dialogAction.dialogProps}>
            {dialogAction.render(ctx)}
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
