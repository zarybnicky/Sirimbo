"use client"

import * as React from "react"
import * as DialogPrimitive from "@radix-ui/react-dialog"
import { X } from "lucide-react"
import { cn } from '@/ui/cn';
import { useControllableState } from '@radix-ui/react-use-controllable-state';
import { FormResultContext } from "@/ui/form";
import { buttonCls } from '@/ui/style';
import { Edit, Plus } from 'lucide-react';
import { DropdownMenuButton } from "@/ui/dropdown";

export const Dialog = ({
  open: maybeOpen,
  onOpenChange: maybeOnOpenChange,
  ...props
}: DialogPrimitive.DialogProps) => {
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

export const DialogTrigger = DialogPrimitive.Trigger

export const StdDialogTrigger = Object.assign(
  React.forwardRef<
    HTMLButtonElement,
    Parameters<typeof buttonCls>[0] & { children?: React.ReactNode }
  >(function Button ({ children, ...props }, ref) {
    return (
      <DialogTrigger asChild>
        <button ref={ref} className={buttonCls({ variant: 'outline', ...props })}>
          {children}
        </button>
      </DialogTrigger>
    );
  }),
  {
    Add: React.forwardRef<
      HTMLButtonElement,
      Parameters<typeof buttonCls>[0] & { text?: React.ReactNode }
    >(function AddButton ({ text = 'Přidat', ...props }, ref) {
      return (
        <DialogTrigger asChild>
          <button ref={ref} className={buttonCls({ variant: 'outline', ...props })}>
            <Plus />
            {text}
          </button>
        </DialogTrigger>
      );
    }),
    Edit: React.forwardRef<
      HTMLButtonElement,
      Parameters<typeof buttonCls>[0] & { text?: React.ReactNode }
    >(function EditButton ({ text = 'Upravit', ...props }, ref) {
      return (
        <DialogTrigger asChild>
          <button ref={ref} className={buttonCls({ variant: 'outline', ...props })}>
            <Edit />
            {text}
          </button>
        </DialogTrigger>
      );
    }),
    Dropdown: React.forwardRef<
      HTMLButtonElement,
      { text?: React.ReactNode }
    >(function DropdownMenuTrigger ({ text = 'Upravit' }, ref) {
      return (
        <DialogTrigger asChild>
          <DropdownMenuButton ref={ref} onSelect={(e) => e.preventDefault()}>
            {text}
          </DropdownMenuButton>
        </DialogTrigger>
      );
    }),
  }
);

export const DialogPortal = ({ children, ...props }: DialogPrimitive.DialogPortalProps) => (
  <DialogPrimitive.Portal {...props}>
    <div className="fixed inset-0 z-40 flex justify-center items-center">
      {children}
    </div>
  </DialogPrimitive.Portal>
)
DialogPortal.displayName = DialogPrimitive.Portal.displayName

export const DialogOverlay = React.forwardRef<
  React.ElementRef<typeof DialogPrimitive.Overlay>,
  React.ComponentPropsWithoutRef<typeof DialogPrimitive.Overlay>
>(({ className, ...props }, ref) => (
  <DialogPrimitive.Overlay
    ref={ref}
    className={cn(
      "fixed inset-0 z-40 bg-black/30 backdrop-blur-sm",
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

export const DialogHeader = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn(
      "flex flex-col space-y-1.5 text-center sm:text-left",
      className
    )}
    {...props}
  />
)
DialogHeader.displayName = "DialogHeader"

export const DialogFooter = ({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) => (
  <div
    className={cn(
      "flex flex-col-reverse sm:flex-row sm:justify-end sm:space-x-2",
      className
    )}
    {...props}
  />
)
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
