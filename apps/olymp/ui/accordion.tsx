import React from 'react';
import classNames from 'classnames';
import * as AccordionPrimitive from '@radix-ui/react-accordion';
import { ChevronDownIcon } from 'lucide-react';
import { cn } from './cn';

export const Accordion = ({
  className,
  children,
  ...props
}: AccordionPrimitive.AccordionSingleProps | AccordionPrimitive.AccordionMultipleProps) => (
  <AccordionPrimitive.Accordion className={cn("bg-neutral-6 rounded-md shadow-[0_2px_10px] shadow-black/5", className)} {...props}>
      {children}
  </AccordionPrimitive.Accordion>
)
Accordion.displayName = AccordionPrimitive.Accordion.displayName;

export const AccordionItem = React.forwardRef<
  React.ElementRef<typeof AccordionPrimitive.Item>,
  React.ComponentPropsWithoutRef<typeof AccordionPrimitive.Item>
>(({ className, children, ...props }, ref) => (
  <AccordionPrimitive.Item
    className={classNames(
      'focus-within:shadow-neutral-12 mt-px overflow-hidden first:mt-0 first:rounded-t last:rounded-b focus-within:relative focus-within:z-10 focus-within:shadow-[0_0_0_2px]',
      className
    )}
    {...props}
    ref={ref}
  >
    {children}
  </AccordionPrimitive.Item>
));
AccordionItem.displayName = AccordionPrimitive.Item.displayName

export const AccordionTrigger = React.forwardRef<
  React.ElementRef<typeof AccordionPrimitive.Trigger>,
  React.ComponentPropsWithoutRef<typeof AccordionPrimitive.Trigger>
>(({ className, children, ...props }, ref) => (
  <AccordionPrimitive.Header className="flex">
    <AccordionPrimitive.Trigger
      className={classNames(
        'cursor-pointer text-accent-12 shadow-neutral-6 hover:bg-neutral-2 group flex h-[45px] flex-1 items-center justify-between bg-white px-5 text-[15px] leading-none shadow-[0_1px_0] outline-none',
        className
      )}
      {...props}
      ref={ref}
    >
      {children}
      <ChevronDownIcon
        className="text-accent-10 ease-[cubic-bezier(0.87,_0,_0.13,_1)] transition-transform duration-300 group-data-[state=open]:rotate-180"
        aria-hidden
      />
    </AccordionPrimitive.Trigger>
  </AccordionPrimitive.Header>
));
AccordionTrigger.displayName = AccordionPrimitive.Trigger.displayName

export const AccordionContent = React.forwardRef<
  React.ElementRef<typeof AccordionPrimitive.Content>,
  React.ComponentPropsWithoutRef<typeof AccordionPrimitive.Content>
>(({ className, children, ...props }, ref) => (
  <AccordionPrimitive.Content
    className={classNames(
      'text-neutral-12 bg-neutral-2 data-[state=open]:animate-accordionSlideDown data-[state=closed]:animate-accordionSlideUp overflow-hidden text-[15px]',
      className
    )}
    {...props}
    ref={ref}
  >
    <div className="py-[15px] px-5">{children}</div>
  </AccordionPrimitive.Content>
));
AccordionContent.displayName = AccordionPrimitive.Content.displayName
