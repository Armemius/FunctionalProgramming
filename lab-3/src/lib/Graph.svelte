<script lang="ts">
  import { onMount } from "svelte";
  import type { Point } from "./types";

  let calculatorContainer: HTMLDivElement | null = null;
  let calculator: any = null;

  let { points } = $props();

  onMount(() => {
    if (!calculatorContainer) return;
    calculator = Desmos.GraphingCalculator(calculatorContainer);
  });

  $effect(() => {
    let sorted_points = [...points]
      .sort((a: Point, b: Point) => a.y - b.y)
      .sort((a: Point, b: Point) => a.x - b.x);

    calculator?.setExpression({
      id: "points",
      type: "table",

      columns: [
        {
          latex: "x",
          values: sorted_points.map((point: Point) => point.x),
        },
        {
          latex: "y",
          values: sorted_points.map((point: Point) => point.y),
          points: true,
          lines: true,
          color: Desmos.Colors.BLACK,
          pointStyle: Desmos.Styles.OPEN,
          lineStyle: Desmos.Styles.DASHED,
          lineOpacity: 0.2,
          pointOpacity: 0.8,
        },
      ],
    });
  });
</script>

<section class="w-full">
  <h3 class="pb-1">График</h3>
  <div bind:this={calculatorContainer} class="w-full h-[500px]"></div>
</section>
