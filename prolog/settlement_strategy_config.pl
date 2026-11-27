:- module(settlement_strategy_config, [priority/2]).

% Utility points. Distances are in tiles, HP in hit points.
priority(route_step, 2).
priority(route_danger, 5).
priority(commitment, 9).
priority(retry_seconds, 15).
priority(no_route_retry_seconds, 5).
priority(critical_hp, 30).
priority(threat_radius, 5).
priority(recovery_radius, 7).
priority(intercept_radius, 6).
priority(intercept_steps, 7).
priority(enemy_urgent_score, 85).
priority(victory_score, 100).
priority(victory_build, 300).
priority(build, 46).
priority(first_building_kind, 26).
priority(repeated_building_kind, 48).
priority(preferred_building, 12).
priority(build_cost, 0.65).
priority(build_points, 1.2).
priority(gather, 30).
priority(tool_supply, 8).
priority(craft, 79).
priority(repair, 43).
priority(critical_repair, 34).
priority(fence, 44).
priority(raid, 10).
priority(urgent_raid, 100).
priority(intercept, 91).
priority(self_defence, 120).
priority(retreat, 230).
