"use client"

import * as React from "react"
import * as DialogPrimitive from "@radix-ui/react-dialog"
import { X } from "lucide-react"
import { cn } from '@/ui/cn';
import { useControllableState } from '@radix-ui/react-use-controllable-state';
import { FormResultContext } from "@/ui/form";
import { buttonCls } from '@/ui/style';
import { Edit, Plus } from 'lucide-react';

export function Dialog({
  open: maybeOpen,
  onOpenChange: maybeOnOpenChange,
  ...props
}: DialogPrimitive.DialogProps) {
  const [open = false, onOpenChange] = useControllableState({
    prop: maybeOpen,
    onChange: maybeOnOpenChange,
    defaultProp: false,
  });
  const formContext = React.useMemo(() => ({ onSuccess() { onOpenChange(false) } }), [onOpenChange]);
  const context = React.useMemo(() => ({ open, onOpenChange }), [open, onOpenChange]);
  return (
    <FormResultContext.Provider value={formContext}>
      <DialogPrimitive.Root {...props} {...context} />
    </FormResultContext.Provider>
  )
}
Dialog.displayName = DialogPrimitive.Root.displayName


export const DialogTrigger = Object.assign(
  function Button({ text, ...props }: Parameters<typeof buttonCls>[0] & { text?: React.ReactNode }) {
    return (
      <DialogPrimitive.Trigger className={buttonCls({ variant: 'outline', ...props })}>
        {text}
      </DialogPrimitive.Trigger>
    );
  },
  {
    Add({ text = 'Přidat', ...props }: Parameters<typeof buttonCls>[0] & { text?: React.ReactNode }) {
      return (
        <DialogPrimitive.Trigger className={buttonCls({ variant: 'outline', ...props })}>
          <Plus />
          {text}
        </DialogPrimitive.Trigger>
      );
    },
    Edit({ text = 'Upravit', ...props }: Parameters<typeof buttonCls>[0] & { text?: React.ReactNode }) {
      return (
        <DialogPrimitive.Trigger className={buttonCls({ variant: 'outline', ...props })}>
          <Edit />
          {text}
        </DialogPrimitive.Trigger>
      );
    },
  }
);

function DialogPortal({ children, ...props }: DialogPrimitive.DialogPortalProps) {
  return <DialogPrimitive.Portal {...props}>
    <div className="fixed inset-0 z-40 flex justify-center items-center">
      {children}
    </div>
  </DialogPrimitive.Portal>
}
DialogPortal.displayName = DialogPrimitive.Portal.displayName

const DialogOverlay = React.forwardRef<
  React.ElementRef<typeof DialogPrimitive.Overlay>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Overlay>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Overlay
    ref={ref}
    className={cn(
      "fixed inset-0 z-40 bg-black/10 backdrop-blur-sm",
      'data-[state=open]:animate-overlayShow data-[state=closed]:animate-overlayHide',
      className
    )}
    {...props}
  />
))
DialogOverlay.displayName = DialogPrimitive.Overlay.displayName

export const DialogContent = React.forwardRef<
  React.ElementRef<typeof DialogPrimitive.Content>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Content>
>(({ className, children, ...props }, ref) => (
  <DialogPortal>
    <DialogOverlay />
    <DialogPrimitive.Content
      ref={ref}
      className={cn(
        "fixed z-40 grid w-full gap-4 rounded-b-lg border border-neutral-7 bg-neutral-1 text-neutral-12 p-6 shadow-lg",
        'data-[state=open]:animate-contentShow data-[state=closed]:animate-contentHide',
        "sm:max-w-lg sm:rounded-lg overflow-y-auto max-h-full",
        className
      )}
      {...props}
    >
      {children}
      <DialogPrimitive.Close className="absolute right-4 top-4 rounded-sm opacity-70 ring-offset-neutral-7 transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-accent-7 focus:ring-offset-2 disabled:pointer-events-none data-[state=open]:bg-accent-5 data-[state=open]:text-white">
        <X className="size-4" />
        <span className="sr-only">Close</span>
      </DialogPrimitive.Close>
    </DialogPrimitive.Content>
  </DialogPortal>
))
DialogContent.displayName = DialogPrimitive.Content.displayName

export function DialogHeader({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return <div
    className={cn(
      "flex flex-col space-y-1.5 text-center sm:text-left",
      className
    )}
    {...props}
  />
}
DialogHeader.displayName = "DialogHeader"

export function DialogFooter({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return <div
    className={cn(
      "flex flex-col-reverse sm:flex-row sm:justify-end sm:space-x-2",
      className
    )}
    {...props}
  />
}
DialogFooter.displayName = "DialogFooter"

export const DialogTitle = React.forwardRef<
  React.ElementRef<typeof DialogPrimitive.Title>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Title>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Title
    ref={ref}
    className={cn(
      "text-lg font-semibold leading-none tracking-tight",
      className
    )}
    {...props}
  />
))
DialogTitle.displayName = DialogPrimitive.Title.displayName

export const DialogDescription = React.forwardRef<
  React.ElementRef<typeof DialogPrimitive.Description>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Description>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Description
    ref={ref}
    className={cn("text-sm text-neutral-10", className)}
    {...props}
  />
))
DialogDescription.displayName = DialogPrimitive.Description.displayName
