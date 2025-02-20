"use client"

import * as React from "react"
import * as PopoverPrimitive from "@radix-ui/react-popover"
import { cn } from '@/ui/cn';
import { X } from 'lucide-react';

export const Popover = PopoverPrimitive.Root

export const PopoverTrigger = PopoverPrimitive.Trigger

function PopoverPortal({ children, ...props }: PopoverPrimitive.PopoverPortalProps) {
  return (
    <PopoverPrimitive.Portal {...props}>
      <div className="fixed inset-0 z-40 flex items-start justify-center sm:items-center">
        {children}
      </div>
    </PopoverPrimitive.Portal>
  );
}
PopoverPortal.displayName = PopoverPrimitive.Portal.displayName

export const PopoverContent = React.forwardRef<
  React.ElementRef<typeof PopoverPrimitive.Content>,
  React.ComponentPropsWithoutRef<typeof PopoverPrimitive.Content>
>(function PopoverContent({ className, children, ...props }, ref) {
  return (
  <PopoverPortal>
    <PopoverPrimitive.Content
      ref={ref}
      className={cn(
        'z-30 data-[side=top]:animate-slideUpAndFade data-[side=bottom]:animate-slideDownAndFade',
        "border border-neutral-7 bg-neutral-1 text-neutral-12 p-6 shadow-lg",
        "sm:max-w-lg rounded-lg overflow-y-auto max-h-full",
        className
      )}
      {...props}
    >
      <PopoverPrimitive.Arrow className="fill-current text-white" />
      {children}
      <PopoverPrimitive.Close className="absolute right-4 top-4 rounded-sm opacity-70 ring-offset-neutral-7 transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-accent-7 focus:ring-offset-2 disabled:pointer-events-none data-[state=open]:bg-accent-5 data-[state=open]:text-white">
        <X className="size-4" />
        <span className="sr-only">Close</span>
      </PopoverPrimitive.Close>
    </PopoverPrimitive.Content>
  </PopoverPortal>
)
});
PopoverContent.displayName = PopoverPrimitive.Content.displayName
