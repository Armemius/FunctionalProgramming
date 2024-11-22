<script lang="ts">
  import { Terminal } from "@xterm/xterm";
  import { onMount } from "svelte";

  const term = new Terminal();
  let terminalElement: HTMLDivElement | null = null;
  let buffer = '';
  let active = true;

  const awaitLock = async () => {
    while (!active) {
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
  };

  export const acquireLock = () => {
    active = false;
    buffer = '';
  };

  export const releaseLock = () => {
    active = true;
  };

  export const write = async (text: string) => {
    term.write(text);
  };

  export const clearBuffer = () => {
    buffer = '';
  };

  export const clearTerminal = () => {
    term.clear();
    term.write('\x1b[2K\r')
  };

  export let readCallback = async (text: string) => {
    console.log(text);
  };

  export let resetCallback = async () => {
    term.write("^C");
  };

  const handleKeydown = async (ev: KeyboardEvent) => {
    ev.preventDefault();
    term.scrollToBottom();

    if (!active || ev.type !== "keydown") {
      return;
    }

    if (ev.key === "Enter") {
      if (ev.shiftKey) {
        buffer += "\n";
        term.write("\n\r");
        return;
      }
      term.write("\n\r");
      await readCallback(buffer);
    } else if (ev.code === "KeyC" && ev.ctrlKey) {
      await resetCallback();
    } else if (ev.key === "Backspace" && buffer.length > 0) {
      buffer = buffer.slice(0, -1);
      term.write("\b \b");
    } else if (ev.code === "KeyL" && ev.ctrlKey) {
      term.clear();
    } else if (ev.key.length === 1) {
      buffer += ev.key;
      term.write(ev.key);
    }
  };

  const initTerminal = () => {
    term.open(terminalElement!!);
    term.attachCustomKeyEventHandler(ev => {
      handleKeydown(ev);
      return false;
    });
  };

  onMount(initTerminal);
</script>

<section>
  <h3 class="pb-1">Терминал</h3>
  <div class="bg-black w-min p-2">
    <div class="w-[728px]" bind:this={terminalElement}></div>
  </div>
</section>

<style>
  :global(.xterm-viewport) {
    overflow-y: hidden !important;
  }
</style>
